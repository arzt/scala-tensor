# tensor4s

##What is it?
tensor4s expands the scala programming language by multi-dimensional arrays (tensors) and 
linear algebra operators aiming to bring 
numerical computation to the scala world using a simple yet powerful syntax.
With that said tensor4s is comparable but yet different to following existing linear algebra libraries:

* [ScalaNLP's Breeze](https://github.com/scalanlp/breeze)
* [ND4J/ND4S](https://nd4j.org/)

tensor4s aims not be a replacement but yet another linear algebra library 
with following features:
#### Implemented in Scala
In my opinion Java syntax is no meant to support linear algebra extensions
in a non-cumbersome way comparable to python's ndarrays. The situations is different with Scala. 
Scala's rich syntax allows a seamingless integration of linear algebra support
using a simple yet powerful syntax known from other languages such as
python and matlab.
 
#### Backed by plain old arrays
tensor4's tensors are backed by plain old java arrays.
This way tensors can be seen as multidimensional containers storing
elements of arbitrary data types. This mean tensors are not limited to
primitive data types such as double or float, but may contain any
data structure, even tensors. 

**Note**: Currently the status of this library is pre alpha. 
The code is likely to pass breaking changes in the future.
Use at your own risk.
##Build and install:
tenor4s uses [SBT](https://www.scala-sbt.org/) for building the library. Clone this git repository, open shell and publish artifacts locally:
```bash
sbt publishLocal
```
Include tensor4s to your library dependencies:
```sbtshell
libraryDependencies ++= Seq(
  "com.github.arzt" %% "scala-tensor" % "0.0.0-SNAPSHOT"
)
```
**Note**: Adding tensor4s to Maven repository is planned for a later stable release.
##Documentation
To Generate scala doc run the following command
```sbtshell
sbt doc
```

##Syntax
###Creating tensors
```scala
//import linear algebra DSL:
import com.github.arzt.tensor.TensorImplicits._
import com.github.arzt.tensor.Tensor
//all examples expect this import

val shape = Vector(2,3)
val data = Array[Int](
  0,1,2,
  3,4,5
)
//All lines create the same 2 rows by 3 cols tensor
val tensor0 = Tensor[Int](shape, data)
val tensor1 = data.asRows(2)
val tensor2 = data.asCols(3)
//The type of the vals is com.github.arzt.tensor.Tensor

```
