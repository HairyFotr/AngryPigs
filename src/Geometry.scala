package AngryPigs

class Vec3(var x:Float, var y:Float, var z:Float) {
    def this() = this(0f,0f,0f);
    
    def getPoints = List(x,y,z);
    def setPoints(v:Vec3):Vec3 = { this.x=v.x; this.y=v.y; this.z=v.z; this }
    def setPoints(p:List[Float]):Vec3 = setPoints(new Vec3(p(0),p(1),p(2)))
    def setPoints(x:Float,y:Float,z:Float):Vec3 = setPoints(new Vec3(x,y,z))
    
    override def clone = new Vec3(x,y,z);
    def map(f:Float=>Float):Vec3 = { setPoints(getPoints.map(f)); this }
    def applyVector(v:Vec3, multi:Float=1):Vec3 = setPoints(this+(v*multi))
    
    def unary_- :Vec3 = new Vec3(-x, -y, -z)
    def +(v:Vec3):Vec3 = new Vec3(x+v.x, y+v.y, z+v.z)
    def -(v:Vec3):Vec3 = new Vec3(x-v.x, y-v.y, z-v.z)
    def +=(v:Vec3) = applyVector(v, +1)
    def +=(f:Float) = this.map(_ + f)
    def -=(v:Vec3) = applyVector(v, -1)
    def -=(f:Float) = this.map(_ - f)
    def *(v:Vec3):Vec3 = new Vec3(x*v.x, y*v.y, z*v.z)
    def *(f:Float):Vec3 = this.clone.map(_ * f)
    def *=(f:Float):Vec3 = this.map(_ * f)
    def /(f:Float):Vec3 = this.clone.map(_ / f)
    def X(v:Vec3):Vec3 = new Vec3(y*v.z - z*v.y, z*v.x - x*v.z, x*v.y - y*v.x)
    def dot(v:Vec3):Float = (new Vec3(x*v.x, y*v.y, z*v.z)).getPoints.reduceLeft(_+_)

    //maybe this needs to be normalized too
    def angle(v: Vec3):Float = (180f/math.Pi * math.acos((this dot v)/v.length)).toFloat;

    def length:Float = math.sqrt(this.clone.map(math.pow(_,2).toFloat).getPoints.reduceLeft(_+_)).toFloat
    def ==(v:Vec3):Boolean = (x==v.x && y==v.y && z==v.z)
    def !=(v:Vec3):Boolean = !(this==v)
    
    // take max or min value
    def takeMax(v:Vec3) {
        if(v.x > x) x = v.x;
        if(v.y > y) y = v.y;
        if(v.z > z) z = v.z;
    }
    def takeMin(v:Vec3) {
        if(v.x < x) x = v.x;
        if(v.y < y) y = v.y;
        if(v.z < z) z = v.z;
    }
    
    // clamp values to some value(e.g. world size)
    private def clamp(p:Float,clamp:Float):Float = if(clamp!=0 && math.abs(p) > clamp) clamp*(p/math.abs(p)) else p
    def clamp(c:Float):Vec3 = this.map(clamp(_,c));
    def clamp(cx:Float,cy:Float,cz:Float):Vec3 = setPoints(clamp(x, cx), clamp(y, cy),clamp(z, cz))

    override def toString = "%.2f, %.2f, %.2f".format(x,y,z);
}

class BoundingBox(var min:Vec3) {
    min = min.clone;
    var max = min.clone;
    def this(v1:Vec3, v2:Vec3) = {
        this(v1.clone);
        this += v2;
    }
    def this(points:List[Vec3]) = {
        this(points(0).clone);
        for(i <- 1 until points.length)
            this += points(i);        
    }
    
    def boxCollide(b:BoundingBox, offset:Vec3=new Vec3):Boolean = {///tolerance
        ((min.x+offset.x <= b.max.x) && (max.x+offset.x >= b.min.x) && 
         (min.y+offset.y <= b.max.y) && (max.y+offset.y >= b.min.y) &&
         (min.z+offset.z <= b.max.z) && (max.z+offset.z >= b.min.z))
    }
    def pointCollide(v:Vec3, offset:Vec3=new Vec3):Boolean = {
        ((min.x+offset.x <= v.x) && (max.x+offset.x >= v.x) && 
         (min.y+offset.y <= v.y) && (max.y+offset.y >= v.y) &&
         (min.z+offset.z <= v.z) && (max.z+offset.z >= v.z))
    }
    
    def +=(v:Vec3):Unit = {
        this.min.takeMin(v);
        this.max.takeMax(v);
    }
    def +=(b:BoundingBox):Unit = {
        this += b.min
        this += b.max
    }
    def ++(b:BoundingBox):BoundingBox = {// merge boxes
        var box = this.clone;
        box += b;
        box;
    }
    def offsetBy(v:Vec3):BoundingBox = {// offset box
        var box = this.clone;
        box.min += v;
        box.max += v;
        box;
    }
    
    override def clone:BoundingBox = new BoundingBox(min.clone,max.clone);
}
