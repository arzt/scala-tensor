# tensor4s

## What is it?

tensor4s expands the scala programming language by multi-dimensional arrays (tensors) and 
linear algebra operators aiming to bring 
numerical computation to the scala world using a simple yet powerful syntax.
With that said tensor4s is comparable but yet different to following existing linear algebra libraries:

* [ScalaNLP's Breeze](https://github.com/scalanlp/breeze)
* [ND4J/ND4S](https://nd4j.org/)

tensor4s aims not be a replacement but yet another linear algebra library 
with following features:

#### Implemented in Scala
Plain Java syntax makes it hard to support simple linear algebra expressions 
comparable to python's nd-arrays. The situations is different with Scala. 
Scala's rich syntax allows for a seamless integration of operators/expressions
known from languages such as python and matlab.
Tensor4s is implemented from scratch in scala, 
bringing the powerful syntax of existing linear algebra library to the scala world.
 
#### Backed by plain old arrays
tensor4's tensors are backed by plain old java arrays.
Tensors can be seen as multidimensional containers storing
elements of arbitrary data types. Tensors are therefore not limited to
primitive data types such as double or float, but may contain any
data structure, even other tensors. 

**Note**: Currently the status of this library is pre alpha. 
The code is likely to pass breaking changes in the future.
Use at your own risk.
## Build and install:
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
## Scaladoc
To Generate scaladoc documentation run the following command
```sbtshell
sbt doc
```

## Syntax
### Creating tensors
```scala
//import linear algebra DSL:
//all following examples expect the presence of this imports
import com.github.arzt.tensor.TensorImplicits._
import com.github.arzt.tensor.Tensor

val shape = Vector(2, 3)
val data = Array[Int](
  0,1,2,
  3,4,5
)
//Each line creates the same 2 by 3 matrix
val tensor0 = Tensor(shape, data)
val tensor1 = data.asRows(2)
val tensor2 = data.asCols(3)
val tensor3 = data.asTensor(2, 3)
//Each tensor has the inferred type Tensor[Int]

//Row/Columns vectors:
val row: Tensor[Boolean] = Array(true, false, false).asRow
val col: Tensor[String] = Array("1", "2", "3").asCol

```

### Reading/Updating single elements
```scala
import com.github.arzt.tensor.TensorImplicits._
import com.github.arzt.tensor.Tensor

val t = 
  Array(
    1,2,
    3,4,
    
    5,6,
    7,8
  )
  .asTensor(2, 2, 2)
```