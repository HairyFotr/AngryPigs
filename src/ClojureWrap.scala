package AngryPigs

class ClojureWrap(ns: String, objName: String) {
  import clojure.lang.{RT,Var}
  import scala.collection.mutable.HashMap

  RT.loadResourceScript(objName+".clj")
  val obj = ns+"."+objName
  val funcs = new HashMap[String, Var]
  
  def /(func: String, a: Any) =
    funcs.getOrElseUpdate(func, RT.`var`(obj, func)).invoke(a.asInstanceOf[Object])
  def /(func: String, a: Any, b: Any) =
    funcs.getOrElseUpdate(func, RT.`var`(obj, func)).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object])
  def /(func: String, a: Any, b: Any, c: Any) =
    funcs.getOrElseUpdate(func, RT.`var`(obj, func)).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object])
  def /(func: String, a: Any, b: Any, c: Any, d: Any) =
    funcs.getOrElseUpdate(func, RT.`var`(obj, func)).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object], d.asInstanceOf[Object])
}
