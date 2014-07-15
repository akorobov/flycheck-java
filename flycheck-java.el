;;; flycheck-java --- On the fly java syntax checking  -*- lexical-binding: t; -*-

;;; Commentary:
;; Copyright (c) 2013,2014 Alexander Korobov 
;; Author: https://github.com/akorobov
;; URL: https://github.com/akorobov/flycheck-java
;; Version: 0.2
;; Package-Requires: ((s "1.9.0") (dash "2.4.0")  (pkg-info "0.4") (cl-lib "0.3") (emacs "24.3") (flycheck "0.17"))
;; License: BSD

;; Provides support for on the fly java syntax checking using flycheck and eclipse
;; batch compiler. Expects java project to use standard project directory layout/structure, able to extract project layout from eclipse project files.

;;; Code:

(require 'xml)
(require 's)
(require 'dash)

(defvar-local flycheck-java-project nil
  "Variable containing java project definition for current file")

(defvar-local  flycheck-java-cmdopts nil
  "Variable containing ecj command line options")

(defconst flycheck-java-eclipse-project-file-name ".project")
(defconst flycheck-java-lisp-project-file-name ".project.el")

;; by default java compiler tries to build transitive closure of source code
;; (all referenced classes) so in large codebases running compiler and parsing
;; errors takes way too much time/resources. no-sourcepath variable controls
;; inclusion of source directories into compiler command. when it is set
;; compiler is given only current classpath (where compiled classes reside).
;; This speeds up compilation/syntax checking at expense of runing on possibly
;; stale data.
(defvar flycheck-java-no-sourcepath t)

(flycheck-def-option-var flycheck-java-ecj-jar-path (expand-file-name  "~/dev/tools/ecj-4.2.jar") java
  "A path to ECJ jar file."
  :type 'string
  :package-version '(flycheck . "0.17")
  :safe #'stringp)

;; Standard java project definition
(defconst flycheck-java-standard-java-project-def
  '(:type :standard
    :source "1.6"
    :target "1.6"
    :options "-warn:+over-ann,uselessTypeCheck -proceedOnError -maxProblems 100"

    ;; source directory -> class directory mappings
    :paths (("src/main/java" . "target/classes") 
            ("src/test/java" . "target/test-classes"))

    ;; library paths to check
    :lib-paths ("lib" "target/dependency"))
    "Standard java project layout and definition.")

(defun flycheck-java-parse-eclipse-classpath (cp-file)
  "Parse Eclipse's .classpath CP-FILE file to extract source/class path mappings and libraries."
  (if cp-file
      (let* ((classpath-entries (xml-get-children (car (xml-parse-file cp-file)) 'classpathentry))
             (paths ())
             (libs ()))
        (mapc (lambda (e)
                (let* ((attrs (xml-node-attributes e))
                       (path (cdr (assq 'path attrs)))
                       (output (cdr (assq 'output attrs)))
                       (kind (cdr (assq 'kind attrs))))
                  (cond ((string= "src" kind)
                        (add-to-list 'paths (cons path output)))
                        ((string= "lib" kind)
                         (add-to-list 'libs path))))) classpath-entries)
        (list :paths paths :libs libs))))

(defun flycheck-java-read-eclipse-project (dir)
  "Parse Eclipse's project file(s) located in DIR directory and return java project definition."
  (let ((ecp (flycheck-java-parse-eclipse-classpath (concat dir  "/.classpath"))))

    ;; return project definition
    (append  (list :project-dir dir
                   :source (plist-get flycheck-java-standard-java-project-def :source)
                   :target (plist-get flycheck-java-standard-java-project-def :target)
                   :options (plist-get flycheck-java-standard-java-project-def :options)
                   ) ecp)))

(defun flycheck-java-match-project-def (dir project-def)
  "Try to match source directory DIR to java project template PROJECT-DEF.  If successful return project type and root directory, otherwise return nil."
  (let* ((paths (-map 'car (plist-get project-def :paths)))
         (pattern (s-concat "\\(.*\\)\\("
                            (--reduce (s-concat acc "\\|" it) paths)
                            "\\)\\(.*\\)"))
         (match (s-match pattern dir)))
    (if match
        (cons (plist-get project-def :type) (nth 1 match))))
  )
;; returns tuple of type and root directory of the project
(defun flycheck-java-find-project (dir)
  ""
  (let ((eclipse-dir (locate-dominating-file dir flycheck-java-eclipse-project-file-name))
        (fmj-dir (locate-dominating-file dir flycheck-java-lisp-project-file-name)))
    (cond
     ((stringp fmj-dir) (cons :custom fmj-dir))
     ((stringp eclipse-dir) (cons :eclipse eclipse-dir))
     (t (flycheck-java-match-project-def dir flycheck-java-standard-java-project-def)))))
    
(defun flycheck-java-get-project-def (dir)
  "Look up project type and root directory for given source path DIR, if successful read, parse and return project definition."
  (let* ((project (flycheck-java-find-project dir))
         (root-dir (cdr project)))
    (pcase (car project)
      (:eclipse (flycheck-java-read-eclipse-project root-dir))
      (:custom
       (cons (cons 'project-dir root-dir)
             (flycheck-java-read-project-def root-dir)))
      (:standard (append `(:project-dir ,root-dir) flycheck-java-standard-java-project-def))
      (:else
       (error "unable to determine project root and type")))))

(defun flycheck-java-expand-lib-path (project-dir lib-path)
  "Given PROJECT-DIR and LIB-PATH directory containing jar/zip files produce list lib-path/lib1.jar ... lib-path/libN.jar of libraries."
  (let ((abs-dir (expand-file-name lib-path project-dir)))
    (when (file-exists-p abs-dir)
      (--map (concat lib-path "/" it)
             (--filter (not (s-contains? "-sources" it))
                       (directory-files abs-dir nil ".*\\(jar\\|zip\\)"))))))

(defun flycheck-java-expand-lib-paths (project-dir lib-paths)
  "Given PROJECT-DIR project root directory and list of relative LIB-PATHS directories produce list of (lib-path1/lib1.jar ... lib-pathN/libM.jar) libraries."
  (when lib-paths
    (--mapcat (flycheck-java-expand-lib-path project-dir it) lib-paths)))

(defun flycheck-java-make-cmd-options (p)
  "Create list of command line options for given project definition P."
  (let* ((root (expand-file-name (plist-get p :project-dir)))
         (paths (plist-get p :paths))
         (src-paths (--map (concat root "/" (car it)) paths))
         (dst-paths (--map (concat root "/" (cdr it)) paths))
         (libs (plist-get p :libs))
         (expanded-libs (if libs libs (flycheck-java-expand-lib-paths root (plist-get p :lib-paths))))
         (source-path (--reduce (concat acc ":" it) src-paths))
         (class-path (--reduce (concat acc ":" it)
                               (-flatten (list dst-paths (--map (concat root "/" it) expanded-libs))))))
    (append (list "-source"
                  (plist-get p :source)
                  "-target"
                  (plist-get p :source)
                  "-classpath" class-path)
            (if flycheck-java-no-sourcepath nil `("-sourcepath" ,source-path)))))

;; checker options initialization
(defun flycheck-java-get-cmd-options ()
  "Pre-compute and store project definition and checker command line in buffer variables."
  (when (not flycheck-java-project)
    (let* ((project-def (flycheck-java-get-project-def default-directory))
           (opts (flycheck-java-make-cmd-options project-def)))
      ;; project definition with pre-built command line options is kept in current-java-project buffer variable
      (setq-local flycheck-java-project project-def)
      (setq-local flycheck-java-cmdopts opts)))
  flycheck-java-cmdopts)


(flycheck-define-checker java
  "Java syntax checker using ecj batch compiler."
  :command ("java" 
            (option "-jar" flycheck-java-ecj-jar-path)
            "-d" "none" "-Xemacs" 
            (eval (flycheck-java-get-cmd-options))
            source)
  :error-patterns 
  ((warning line-start (file-name) ":" line ": warning:" 
            (message (zero-or-more not-newline)) line-end)
   (error line-start (file-name) ":" line ": error:" 
          (message (zero-or-more not-newline)) line-end))
  :modes java-mode)

(add-to-list 'flycheck-checkers 'java)

(provide 'flycheck-java)
;;; flycheck-java ends here
