package AngryPigs

import math._

object Vec3 { 
  def apply(): Vec3 = new Vec3
  def apply(x: Float, y: Float, z: Float): Vec3 = new Vec3(x, y, z) 
}
class Vec3(var x: Float, var y: Float, var z: Float) {
  def this() = this(0f, 0f, 0f)
  
  private def setPoints(v: Vec3) { x=v.x; y=v.y; z=v.z; }
  private def setPoints(x: Float, y: Float, z: Float) { this.x=x; this.y=y; this.z=z; }
  private def setPoints(p: Array[Float]): Unit = setPoints(p(0), p(1), p(2))
  
  override def clone: Vec3 = Vec3(x,y,z)
  private def each(f: Float => Float) { x = f(x); y = f(y); z = f(z); }
  private def map(f: Float => Float): Vec3 = { val out = this.clone; out.each(f); out }
  def applyVector(v: Vec3, multi: Float = 1): Unit = setPoints(this + (v * multi))
  
  def unary_- : Vec3      = Vec3(-x, -y, -z)
  def +(v: Vec3): Vec3    = Vec3(x+v.x, y+v.y, z+v.z)
  def -(v: Vec3): Vec3    = Vec3(x-v.x, y-v.y, z-v.z)
  def +=(v: Vec3): Unit   = setPoints(this + v)
  def +=(f: Float): Unit  = this.each(_ + f)
  def -=(v: Vec3): Unit   = setPoints(this + (-v))
  def -=(f: Float): Unit  = this.each(_ - f)
  def *(v: Vec3): Vec3    = Vec3(x*v.x, y*v.y, z*v.z)
  def *(f: Float): Vec3   = this.map(_ * f)
  def *=(f: Float): Unit  = this.each(_ * f)
  def /(f: Float): Vec3   = this.map(_ / f)
  def X(v: Vec3): Vec3    = Vec3(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)
  def dot(v: Vec3): Float = x*v.x + y*v.y + z*v.z

  //maybe this needs to be normalized too
  def angle(v: Vec3): Float = (180f/Pi * acos((this dot v)/v.length)).toFloat

  def length: Float = sqrt(this dot this).toFloat
  def ==(v: Vec3): Boolean = x == v.x && y == v.y && z == v.z
  def !=(v: Vec3): Boolean = !(this == v)
  
  def maxCoords(v: Vec3): Vec3 = Vec3(max(v.x, x), max(v.y, y), max(v.z, z))
  def minCoords(v: Vec3): Vec3 = Vec3(min(v.x, x), min(v.y, y), min(v.z, z))
  
  // clamp values to some value(e.g. world size)
  private def clamp(p: Float, clamp: Float): Float = if(clamp != 0 && abs(p) > clamp) clamp * (p / abs(p)) else p
  def clamp(c: Float): Unit = this.each(clamp(_, c))
  def clamp(cx: Float, cy: Float, cz: Float): Unit = setPoints(clamp(x, cx), clamp(y, cy), clamp(z, cz))

  override def toString: String = "%.2f, %.2f, %.2f".format(x,y,z)
}

class BoundingBox(var min:Vec3) {
    min = min.clone
    var max = min.clone
    
    def this(v1:Vec3, v2:Vec3) = {
        this(v1.clone)
        this += v2
    }
    def this(points:List[Vec3]) = {
        this(points(0).clone)
        for(i <- 1 until points.length) this += points(i)
    }
    
    def boxCollide(b:BoundingBox, offset:Vec3=Vec3()):Boolean = {///@ tolerance
        ((min.x+offset.x <= b.max.x) && (max.x+offset.x >= b.min.x) && 
         (min.y+offset.y <= b.max.y) && (max.y+offset.y >= b.min.y) &&
         (min.z+offset.z <= b.max.z) && (max.z+offset.z >= b.min.z))
    }
    def pointCollide(v:Vec3, offset:Vec3=Vec3()):Boolean = {
        ((min.x+offset.x <= v.x) && (max.x+offset.x >= v.x) && 
         (min.y+offset.y <= v.y) && (max.y+offset.y >= v.y) &&
         (min.z+offset.z <= v.z) && (max.z+offset.z >= v.z))
    }
    
    def +=(v:Vec3):Unit = {
        this.min = min.minCoords(v)
        this.max = max.maxCoords(v)
    }
    def +=(b:BoundingBox):Unit = {
        this += b.min
        this += b.max
    }
    def ++(b:BoundingBox):BoundingBox = {// merge boxes
        var box = this.clone
        box += b
        box
    }
    def offsetBy(v:Vec3):BoundingBox = {// offset box
        var box = this.clone
        box.min += v
        box.max += v
        box
    }
    
    override def clone:BoundingBox = new BoundingBox(min.clone,max.clone)
}
