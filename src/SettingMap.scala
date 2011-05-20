package AngryPigs

import scala.collection.mutable.HashMap;

class SettingMap[A] extends HashMap[A,Any] {
    def getBoolean(key:A):Boolean = getOrElse(key, true).asInstanceOf[Boolean];
    def getInt(key:A):Int = getOrElse(key, -1).asInstanceOf[Int];
    def getFloat(key:A):Float = getOrElse(key, 0f).asInstanceOf[Float];
    def get[B](key:A):B = getOrElse(key, null).asInstanceOf[B];
    
}

