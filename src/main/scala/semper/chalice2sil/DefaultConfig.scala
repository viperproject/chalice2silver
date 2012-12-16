package semper.chalice2sil

import java.util.Properties
import java.nio.file.{Files, Paths, Path}
import java.io.FileNotFoundException

/**
  * @author Christian Klauser
  */
object DefaultConfig {
  lazy val default : Properties = {
    val str = classOf[ProgramOptions].getClassLoader.getResourceAsStream("default.properties")
    val props = new Properties
    if(str == null) {
      System.err.println("Cannot find default configuration for Chalice2SIL tests (default.properties is missing).");
    } else {
      props.load(str)
    }
    props
  }
  lazy val z3path : Path = {
    val paths = resolveEnvVars(default.getProperty("chalice2sil.z3paths"))
    paths
      .split(';')
      .map(Paths.get(_, "z3.exe"))
      .find(p => {
      Files.exists(p)
    }) match {
      case None => throw new FileNotFoundException("Cannot find z3.exe in any of the following paths: %s".format(paths))
      case Some(p) => p
    }
  }

  /*
  * Returns input string with environment variable references expanded, e.g. $SOME_VAR or ${SOME_VAR}
  * Stolen from http://stackoverflow.com/questions/2263929/regarding-application-properties-file-and-environment-variable
  */
  private[this] def resolveEnvVars(input : String) : String = {
    import java.util.regex._
    if(null == input) {
      return null;
    }
    // match ${ENV_VAR_NAME} or $ENV_VAR_NAME
    val p = Pattern.compile("\\$\\{([^\\}]+)\\}|\\$(\\w+)");
    val m = p.matcher(input) // get a matcher object
    val sb = new StringBuffer()
    while(m.find()) {
      val envVarName = if(null == m.group(1)) m.group(2) else m.group(1)
      val envVarValue = System.getenv(envVarName)
      val escapedPattern = (if(null == envVarValue) "" else envVarValue.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("$", "\\$"))
      m.appendReplacement(sb, escapedPattern)
    }
    m.appendTail(sb);
    sb.toString;
  }

}
