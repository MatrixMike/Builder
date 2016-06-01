# Builder

An experimental build tool for Java projects. 
Based around parsing a project file and invoking various command line utilities such as javac, jar etc.
The project file is divided into several sections. First is the 'env' section, content to-be-determined, but probably 
'meta' information about the project. Next is a list of libray declarations in Maven co-ordinate format. This is followed by the module name declaration in the form of name:<Module name>. This  can then be followed by a number of optional key:value declarations. Examples being jarName, main, destPath. The final section  is the   'deploy' section. Here is an example.

```
project{
    env{
      //env settings. TBD.
      }
    build{
      module{
         deps{
		     cglib:cglib:3.1
		     HTTPClient:HTTPClient:0.3-3}
         name      : moduleA
         destPath  : dest/aFolder
         main      : com.dummy.test.RunIt
      }
      module{
         deps{ }
         name      : moduleB
         destPath  : dest/aDifferentFolder/
         main      : com.dummy.test.RunItAgain
      }
    deploy{
      //TBD.
    }
}
```



