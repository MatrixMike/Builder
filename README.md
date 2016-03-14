# Builder

An experimental build tool for Java projects.  Primarily an exercise in learning Haskell. 
Based around parsing a project file and invoking various commandline utilities such as javac, jar etc.
The project file is divided into several sections. First is the 'env' section, content to-be-determined, but probably 
'meta' information about the project.
Next is the declarations in Maven co-ordinate format. The module must have a name declaration in the form of
name:<Module name>. Thsi can then be followed by a number of optional key:value declarations. Examples being jarName, main, destPath. Fianlly at the end of 
the text is the  'deploy' section.

Here is an example.
=
```
project{
    env{
      //env settings. TBD.
      }
    build{
      module{
         deps{
         	org.apache.felix:org.apache.felix.configadmin:1.8.8
		      cglib:cglib:3.1
		      HTTPClient:HTTPClient:0.3-3
		    }
         name      : moduleA
         destPath  : dest/aFolder
         main      : com.dummy.test.RunIt
      }
      module{
         deps{
         	org.apache.felix:org.apache.felix.configadmin:1.8.8
		      cglib:cglib:3.1
		    }
         name      : moduleB
         destPath  : dest/aDifferentFolder/
         main      : com.dummy.test.RunItAgain
      }
    deploy{
      //TBD.
    }
}
```



