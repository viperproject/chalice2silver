package ch.ethz.inf.pm.semper.chalice2sil

import org.junit.runner.notification.RunNotifier
import java.nio.file.{Path, Paths, Files}
import scala.collection.JavaConversions._
import org.junit.runner.{Description, Runner}
import org.junit.runners.ParentRunner
import java.lang.reflect.{InvocationTargetException, Method}
import org.junit.runners.model.{TestClass, FrameworkMethod, Statement}
import org.junit.rules.{RunRules, TestRule}
import org.junit.{Assert, Rule, Ignore, Test}

/**
 * Author: Christian Klauser
 */

class Chalice2SilRunner(backingClass : Class[T] forSome {type T}) extends ParentRunner[BackedChaliceTest](backingClass) {

  val directoryPath = Paths get {
    val dC = Annotations.Directory
    val dirA = backingClass.getAnnotation(classOf[Directory])
    if (dirA != null && dirA.directory() != null)
      dirA.directory()
    else
      backingClass.getName.replace(".","/")
  }
  
  val testCases = {
    val testMethods =
      backingClass.getMethods
        .toStream
        .filter(_.getAnnotation(classOf[Test]) != null)
        .map(m => (m, m.getAnnotation(classOf[Source]) match {
            case s if s != null && s.fileName() != null => Paths.get(s.fileName())
            case _ => Paths.get(m.getName + ".chalice")
          })
        )
        .toSeq
    val explicitSourceFiles = testMethods.view.map(_._2).toSet
    val implicitSourceFiles = Nil //Files.newDirectoryStream(Paths.get(directoryPath)).filterNot(explicitSourceFiles.contains(_)) //TODO: support implicit source files
    
    testMethods.map(t => new BackedChaliceTest(backingClass,t._2,Some(new FrameworkMethod(t._1)))) ++ implicitSourceFiles.map(new BackedChaliceTest(backingClass,_))
  }

  lazy val getChildren = testCases:java.util.List[BackedChaliceTest]

  protected def describeChild(child : BackedChaliceTest) = child.description

  protected def runChild(child : BackedChaliceTest, notifier : RunNotifier) {
    child.backingMethod match {
      case Some(m) if m.getAnnotation(classOf[Ignore]) != null =>
        notifier.fireTestIgnored(child.description)
      case _ => runLeaf(mkStatement(child), child.description, notifier)
    }
  }

  private class Fail(e : Throwable) extends Statement {
    override def evaluate() { throw e }
  }
  
  private def unwrapReflectionException[T](f : => T):T = try {
    f
  } catch {
    case e:InvocationTargetException => throw e.getTargetException
  }
  
  protected def mkStatement(testCase : BackedChaliceTest) : Statement ={
    val testObj = try {
      unwrapReflectionException(getTestClass.getOnlyConstructor.newInstance():Any)
    } catch {
      case e => return new Fail(e)
    }

    val invokeStmt = new Statement {
      def evaluate() {
        import Assert._
        val opts = new ProgramOptions
        opts.chaliceFiles.add(directoryPath.resolve(testCase.sourceFile).toString)
        opts.printSil = true

        Program.invokeChalice(opts) match {
          case None =>
            fail("Chalice failed to parse/typecheck file %s. See Stdout for more details.".format(testCase.sourceFile))
          case Some(p) =>
            val (silProgram, messages) = Program.translateToSil(opts, p)
            messages.view.filter(_.severity.indicatesFailure).foreach(m => fail("Detected message that indicates failure: %s".format(m)))

            Console.out.println(silProgram.toString)
            
            //now invoke backing method
            testCase.backingMethod.foreach(m => {
              m.invokeExplosively(testObj, silProgram, messages)
            })
        }
      }
    }
    withTestRules(testCase, testObj,invokeStmt)
  }

  private def getAnnotatedMethodValues[A <: java.lang.annotation.Annotation,T](test : Any,annotationClass : Class[A], valueClass : Class[T]) : Seq[T] = {
    getTestClass.getAnnotatedMethods(annotationClass).map(_.invokeExplosively(test)).filter(valueClass.isInstance(_))
      .map(_.asInstanceOf[T])
  } 
  
  protected def getTestRules(target : Any) = {
    getAnnotatedMethodValues(target,classOf[Rule],classOf[TestRule]) ++ getTestClass.getAnnotatedFieldValues(target,classOf[Rule],classOf[TestRule])
  }

  protected def withTestRules(child : BackedChaliceTest, testObj : Any, statement : Statement) = {
    val rules = getTestRules(testObj)
    if(rules.isEmpty)
      statement
    else
      new RunRules(statement,rules,child.description)
  }
}