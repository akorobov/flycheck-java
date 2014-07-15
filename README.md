
flycheck-java
=============
### Description

flycheck-java is on-the-fly java syntax [flycheck](https://github.com/flycheck/flycheck) checker using Eclipse's [batch compiler](http://help.eclipse.org/juno/index.jsp?topic=%2Forg.eclipse.jdt.doc.user%2Ftasks%2Ftask-using_batch_compiler.htm).

flycheck-java can figure out project structure/layout from Eclipse project files(.project,.classpath), otherwise it assumes [standard java project structure](http://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html).

This checker is meant to be used as auxiliary tool together with external build system (ide, mvm, ant, etc).

### Customization 

`flycheck-java-ecj-jar-path` option has to configured to point to ECJ jar file, you can customize it using `M-x customize-variable' or programmatically:

```
(add-hook 'java-mode-hook
          (lambda () (setq flycheck-java-ecj-jar-path "/path/to/ecj-4.2.jar")))
```


