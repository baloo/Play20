package play.core.stylus

import java.io._
import play.api._

object StylusCompiler {

  import org.mozilla.javascript._
  import org.mozilla.javascript.tools.shell._

  import scala.collection.JavaConverters._

  import scalax.file._

  private def compiler(minify: Boolean) = {
    val ctx = Context.enter
    val global = new Global; global.init(ctx)
    val scope = ctx.initStandardObjects(global)

    val wrappedStylusCompiler = Context.javaToJS(this, scope)
    ScriptableObject.putProperty(scope, "StylusCompiler", wrappedStylusCompiler)

    ctx.evaluateString(scope,
      """
                var timers = [],
                    window = {
                        document: {
                            getElementById: function(id) { 
                                return [];
                            },
                            getElementsByTagName: function(tagName) {
                                return [];
                            }
                        },
                        location: {
                            protocol: 'file:', 
                            hostname: 'localhost', 
                            port: '80'
                        },
                        setInterval: function(fn, time) {
                            var num = timers.length;
                            timers[num] = fn.call(this, null);
                            return num;
                        }
                    },
                    document = window.document,
                    location = window.location,
                    setInterval = window.setInterval;

            """,
      "browser.js",
      1, null)
    ctx.evaluateString(scope,
"""
if(!String.prototype.trim) {
  String.prototype.trim = function () {
    return this.replace(/^\s+|\s+$/g,'');
  };
}

if(!String.prototype.trimRight) {
  String.prototype.trimRight = function () {
    return this.replace(/\s+$/g,'');
  };
}

if(!Object.keys) Object.keys = function(o){
 if (o !== Object(o))
      throw new TypeError('Object.keys called on non-object');
 var ret=[],p;
 for(p in o) if(Object.prototype.hasOwnProperty.call(o,p)) ret.push(p);
 return ret;
}

if ( !Array.prototype.reduce ) {
  Array.prototype.reduce = function reduce(accumulator){
    var i, l = this.length, curr;

    if(typeof accumulator !== "function") // ES5 : "If IsCallable(callbackfn) is false, throw a TypeError exception."
      throw new TypeError("First argument is not callable");

    if((l == 0 || l === null) && (arguments.length <= 1))// == on purpose to test 0 and false.
      throw new TypeError("Array length is 0 and no second argument");

    if(arguments.length <= 1){
      curr = this[0]; // Increase i to start searching the secondly defined element in the array
      i = 1; // start accumulating at the second element
    }
    else{
      curr = arguments[1];
    }

    for(i = i || 0 ; i < l ; ++i){
      if(i in this)
        curr = accumulator.call(undefined, curr, this[i], i, this);
    }

    return curr;
  };
}
""",
      "stylus-compatibility.js",
      1, null)

    ctx.evaluateReader(scope, new InputStreamReader(
      this.getClass.getClassLoader.getResource("stylus.js").openConnection().getInputStream()),
      "stylus.js",
      1, null)
    ctx.evaluateString(scope,
      """
var compile = function(source) {
  options = {}
  options.filename = String(source);
  options._imports = [];

  var content = String(StylusCompiler.readContent(source))

  var style = stylus(content, options);

  // This is dangerous as hell
  // But style.render do not return what callback function returns
  var generatedContent = ""

  style.render(function(err, css){
    if (err) {
      //java.lang.System.out.println(css.input);
      java.lang.System.out.println(css.filename);
      java.lang.System.out.println(css.lineno.toString());
      java.lang.System.out.println(css);
      java.lang.System.out.println(err);
    } else {
      generatedContent = css;
    }
  });

  // Hopefully rhino is only running one thread
  return generatedContent;
}
            """,
      "compiler.js",
      1, null)
    val compilerFunction = scope.get("compile", scope).asInstanceOf[Function]

    Context.exit

    (source: File) => {
      println(source)
      val result = Context.call(null, compilerFunction, scope, scope, Array(source))
      val css = Context.toString(result)

      css
    }
  }

  private lazy val debugCompiler = compiler(false)

  private lazy val minCompiler = compiler(true)

  def compile(source: File, minify: Boolean): (String, Seq[File]) = {
    println(source)
    try {
      val compiledSource = minify match {
        case true => minCompiler(source)
        case false => debugCompiler(source)
      }
      (compiledSource, Seq[File]())
    } catch {
      case e: JavaScriptException => {
        val error = e.getValue.asInstanceOf[Scriptable]

        throw CompilationException(
          ScriptableObject.getProperty(error, "message").asInstanceOf[String],
          new File(ScriptableObject.getProperty(error, "filename").asInstanceOf[String]),
          ScriptableObject.getProperty(error, "line").asInstanceOf[Double].intValue,
          ScriptableObject.getProperty(error, "column").asInstanceOf[Double].intValue)

      }
    }
  }

  def readContent(file: File): String = Path(file).slurpString.replace("\r", "")
  def resolve(originalSource: File, imported: String) = new File(originalSource.getParentFile, imported)

}

case class CompilationException(message: String, lessFile: File, atLine: Int, atColumn: Int) extends PlayException(
  "Compilation error", message) with PlayException.ExceptionSource {
  def line = Some(atLine)
  def position = Some(atColumn)
  def input = Some(scalax.file.Path(lessFile))
  def sourceName = Some(lessFile.getAbsolutePath)
}

