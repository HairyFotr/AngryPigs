package AngryPigs

import clojure.lang._
import clojure.core._

class ClojureWrap(ns:String,obj:String) {
    RT.loadResourceScript(obj+".clj");  
    
    //@morda bi se dal s parcialno funkcijo obj/"func"(args) in bi invoke prevzel vse (args)
    def /(func:String, a:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object]))
    def /(func:String, a:Any, b:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object]))
    def /(func:String, a:Any, b:Any, c:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object]))
    def /(func:String, a:Any, b:Any, c:Any, d:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object], d.asInstanceOf[Object]))
}

