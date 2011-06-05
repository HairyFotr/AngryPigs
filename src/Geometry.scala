package AngryPigs

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU
import scala.collection.mutable.{ListBuffer,HashMap};


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

abstract class BasicModel {
    var (pos,rot,scale) = (new Vec3, new Vec3, new Vec3(1f,1f,1f));
    var visible = true;

    def setPosition(x:Float,y:Float,z:Float) = { pos = new Vec3(x,y,z); }
    def setPosition(v:Vec3) = { pos = v.clone }
    def setRotation(x:Float,y:Float,z:Float) = { rot = new Vec3(x,y,z); }
    def setRotation(v:Vec3) = { rot = v.clone }
    def setScale(x:Float,y:Float,z:Float) = { scale = new Vec3(x,y,z) }
    def setScale(v:Vec3) = { scale = v.clone }
    
    def doTranslate {
        GL11.glTranslatef(pos.x, pos.y, pos.z);
    }
    def doRotate {
        if(rot.z!=0) GL11.glRotatef(rot.z, 0, 0, 1);
        if(rot.y!=0) GL11.glRotatef(rot.y, 0, 1, 0);
        if(rot.x!=0) GL11.glRotatef(rot.x, 1, 0, 0);        
    }
    def doScaling {
        GL11.glScalef(scale.x, scale.y, scale.z);
    }

    def doTransforms {
        doTranslate
        doRotate
        doScaling
    }

    def render;

    override def toString:String = "p:("+pos.toString+"), " + "r:("+rot.toString+"), " + "s:("+scale.toString+")";
}

// doesn't care about points and stuff
class DisplayModel(var renderfunc:()=>Unit, var idfunc:(DisplayModel,SettingMap[String])=>Int = null) extends BasicModel with Properties {
    def this() = this(()=>null)
    var compiled = false;
    
    var vector = new Vec3;
    var vector2 = new Vec3;

    var displayList:Int = -1;    
    
    // foo values
    properties += "graphics" -> -1;
    properties += "fatlines" -> true;
    
    def forceCompile() {
        import Global._
        displayList = GL11.glGenLists(1);
        GL11.glNewList(displayList,GL11.GL_COMPILE);
        renderfunc();
        GL11.glEndList;
        compiled = true;
        properties += "graphics" -> settings.get[Int]("graphics");
        properties += "fatlines" -> settings.get[Boolean]("fatlines");
        
        try { id() } catch {
            case e:NullPointerException =>
                compileCache += rand.nextInt() -> displayList
                //reset();
        }
    }
    
    
    // copied from scala 2.9 src/library/Predef.scala for scala 2.8 compatibility
    // ... then edited :P
    implicit def Integer2int(x: java.lang.Integer):Int = x.asInstanceOf[Int];
    def id(props:SettingMap[String]=this.properties):Int = 
        if(idfunc!=null) 
            idfunc(this,props);
        else 
            Integer2int(null); //ya, rly :P
    
    var compileCache = new HashMap[Int,Int];
    
    // adds compile cache to compile()
    def compile() {
        try {
            var cid = id(Global.settings);
            
            displayList = compileCache.getOrElseUpdate(cid, {
                forceCompile()
                displayList
            })
            properties += "graphics" -> Global.settings.get[Int]("graphics");
            properties += "fatlines" -> Global.settings.get[Boolean]("fatlines");
        } catch {
            case e:NullPointerException => 
                forceCompile();
        }
    }
    
    def reset() = {
        if(compileCache.size>1) {
            var count = 0;
            val tasks = Global.settings.get[ListBuffer[()=>Unit]]("tasks");
            compileCache.foreach {
                case (_,listid) => if(listid != displayList) {
                    count += 1;
                    tasks += (()=>{GL11.glDeleteLists(listid, 1)})
                }
            }
            println("added delete tasks: "+count);
            compileCache.clear();
        }
    }

    override def clone:DisplayModel = {
        val res = new DisplayModel(this.renderfunc)
        res.pos = this.pos.clone
        res.rot = this.rot.clone
        res.scale = this.scale.clone
        res
    }
    
    override def render() {
        if(!visible) return;
        
        GL11.glPushMatrix;
        doTransforms

        if(displayList == -1 || !GL11.glIsList(displayList)) {
            renderfunc();
            displayList = -1;
        } else {
            GL11.glCallList(displayList);
        }
            
        GL11.glPopMatrix;
    }    
}

class GeneratorModel(generator:()=>Object, draw:Object=>Unit, _idfunc:(DisplayModel,SettingMap[String])=>Int = null) extends DisplayModel {
    var data:Object = generator();
    renderfunc = ()=>{draw(data);()}
    idfunc = _idfunc;
    
    def regenerate() = {
        data = generator();
        compile();
    }
    
    // make a data constructor, so clone has same data. (eliminate generator in static constructor)
    override def clone:GeneratorModel = {
        val res = new GeneratorModel(generator, draw)
        res.pos = this.pos.clone
        res.rot = this.rot.clone
        res.scale = this.scale.clone
        res
    }
}

class TrailModel(var points:List[Vec3]) 
    extends GeneratorModel(()=>{points.map(_.clone)}, 
        (data:Object)=>{
            import org.lwjgl.opengl.GL11._
            import Global._

            val points = data.asInstanceOf[List[Vec3]];
            glColor3f(1f,1f,1f);

            for(i <- 1 until points.length) if(i%2==1) {
                val vecA = points(i-1);
                val vecB = points(i);
                
                val z = new Vec3(0,0,1)
                val p = vecA - vecB
                val cross = z X p
                val angle = z angle p
                
                glPushMatrix
                glTranslatef(vecB.x,vecB.y,vecB.z);
                glRotatef(angle,cross.x,cross.y,cross.z);
                gluQuadrics.cylinder.draw(0.075f,0.075f, p.length, 4,1);
                glPopMatrix
            }
/*
            glBegin(GL_LINES)
            for(p <- points) glVertex3f(p.x, p.y, p.z);
            glEnd;*/
        }) {
        
    def +=(v:Vec3) = {
        data = data.asInstanceOf[List[Vec3]] ++ List(v.clone);
        compile();
    }
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
        var t = this.clone;
        t += b.min;
        t += b.max;
        t;
    }
    def offsetBy(v:Vec3):BoundingBox = {// offset box
        var t = this.clone;
        t.min += v;
        t.max += v;
        t;
    }
    
    override def clone:BoundingBox = new BoundingBox(min.clone,max.clone);
}

class Branch(var parent:Branch) extends Properties {
    var diffVec:Vec3 = new Vec3;
    var rootVec:Vec3 = new Vec3;
    def destVec:Vec3 = rootVec + diffVec;

    var depth = 1;
    var children = new ListBuffer[Branch];

    setParent(parent);

    def setParent(p:Branch) {
        if(p != null) {
            p.children += this
            depth = p.depth+1;
        }
        parent = p;
    }
    def addChild(c:Branch) {
        if(this eq c) return;
        c.setParent(this);
    }
    def detach(){
        if(parent!=null) {
            parent.children -= this;
            this.setParent(null);            
        }
    }
    
    def doAll(f:Branch=>Unit):Unit = {
        f(this);
        children.foreach(_.doAll(f));
    }
    def doWhile(w:Branch=>Boolean, f:Branch=>Unit):Unit = {
        f(this);
        if(w(this)) children.foreach(_.doWhile(w, f));
    }

    def print():Unit = {
        println(depth+" "*(depth*2) + rootVec +" -- " + diffVec)
        children.foreach(_.print())
    }
    
    def render() = {
        import org.lwjgl.opengl.GL11._
        import Global._
        val branch = this // I'm lazy :P
        if(branch.depth <= settings.get[Int]("maxdepth")) {
            if(settings.get[Boolean]("fatlines")) {
                val vecA = branch.rootVec;
                val vecB = branch.destVec;
                
                val z = new Vec3(0,0,1)
                val p = vecA - vecB
                val cross = z X p
                val angle = z angle p
                
                glPushMatrix
                glTranslatef(vecB.x,vecB.y,vecB.z);
                glRotatef(angle,cross.x,cross.y,cross.z);
                glColor3f(0.7f,0.2f,0f);
                val fatness = branch.properties.get[Float]("fatness");
                gluQuadrics.cylinder.draw(fatness/branch.depth,(fatness*2)/branch.depth, branch.diffVec.length, settings.get[Int]("graphics")*5,1);
                if(branch.properties.get[Boolean]("hasLeaf")) {
                    /*if(settings.get[Int]("graphics")==1){
                        glRotatef(180+45, 0,0,1)
                        glTranslatef(0,-0.2f+0.8f*0.25f,0)
                        glScalef(1*0.25f,1.6f*0.25f,1*0.25f)
                        glColor3f(0.9f,0.9f,0.4f)
                        glBegin(GL_QUADS)
                        glVertex3f(0,0,0);
                        glVertex3f(0,1,0);
                        glVertex3f(1,1,0);
                        glVertex3f(1,0,0);
                        glEnd;
                    }*/
                    glScalef(1,1.6f,1)
                    glColor3f(0.2f,0.8f,0.1f)
                    branch.properties.get[Int]("treekind") match {
                        case 0 => 
                            glTranslatef(0,-0.17f,0)
                            glRotatef(rand.nextFloat()*12-rand.nextFloat()*12, 0,0,1)
                            gluQuadrics.disk.draw(0,0.175f, settings.get[Int]("graphics")*6,1)
                        case 1 => 
                            glTranslatef(0,-0.17f,0)
                            gluQuadrics.disk.draw(0,0.175f, 6+rand.nextInt(5),1)
                        case 2 => 
                            glColor3f(0.15f,0.75f,0.075f)
                            glTranslatef(0,-0.13f,0)
                            glRotatef(180+rand.nextFloat()*6-rand.nextFloat()*6, 0,0,1)
                            gluQuadrics.disk.draw(0,0.175f, 5,1)
                        case _ => 
                            gluQuadrics.disk.draw(0,0.175f, 5,1)
                            glRotatef(45, 0,0,1);
                            glTranslatef(0,-0.1f,0)
                            gluQuadrics.disk.draw(0,0.175f, 4,1)
                    }
                }
                glPopMatrix;
            } else {
                glColor3f(0.7f,0.2f,0f);
                glBegin(GL_LINES)
                glVertex3f(branch.rootVec.x,
                           branch.rootVec.y,
                           branch.rootVec.z);                        
                
                glVertex3f(branch.destVec.x,
                           branch.destVec.y,
                           branch.destVec.z)
                glEnd;
            }
        }
    }
}

class Camera extends BasicModel {
    // default projection 
    var perspective = false
    var (near,far) = (1f,30f) // near, far clipping plane
    var (fov,aspectRatio) = (45f,4/3f) // perspective stuff
    var (minX,minY,maxX,maxY) = (-1f,-1f, 1f, 1f) // ortho stuff
    var projectionChanged = true; // do we need to remake projection matrix
    var vector = new Vec3;
    var angle = new Vec3;

    // set a perspective projection
    def setPerspective(fv:Float, ar:Float, n:Float, f:Float) {
        perspective=true
        fov=fv
        aspectRatio=ar
        near=n
        far=f
        projectionChanged = true
    }
  
    // set an ortographic projection
    def setOrtho(mx:Float, my:Float, Mx:Float, My:Float, n:Float, f:Float) {
        perspective=false
        minX=mx
        minY=my
        maxX=Mx
        maxY=My
        near=n
        far=f
        projectionChanged = true
    }
    
    private var lookAtV = new Vec3
    def lookAt(v:Vec3) = lookAtV = v.clone;
    def lookAt(m:BasicModel) = lookAtV = m.pos.clone;
        
    override def render {
        // setup projection matrix stack
        if(projectionChanged) {
            projectionChanged = false;
            GL11.glMatrixMode(GL11.GL_PROJECTION);
            GL11.glLoadIdentity();
            if(perspective) {
                // perspective projection
                GLU.gluPerspective(fov,aspectRatio, near,far);
            } else {
                // orthographic projection 
                GL11.glOrtho(minX,maxX, minY,maxY, near,far);
            }
        }

        // model view stack 
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity();
        GLU.gluLookAt(pos.x,pos.y,pos.z,             // camera position
                      lookAtV.x,lookAtV.y,lookAtV.z, // look-at vector
                      0,1,0)                         // up vector 
    }
}

class ModelLink(var m1:BasicModel, var m2:BasicModel) {
    var vector = new Vec3;
    var vector2 = new Vec3;
    
    def this(m1:BasicModel,m2:BasicModel,v:Vec3, v2:Vec3) {
        this(m1,m2)
        vector = v;
        vector2 = v2;
    }
    def this(m1:BasicModel,m2:BasicModel,v:Vec3) {
        this(m1,m2, v,new Vec3)
    }
    
    private var linked = true;
    def isLinked = linked;
    def breakLink() { linked = false }
    def forgeLink() { linked = true }
    
    def applyLink {
        if(linked) {
            m2.setPosition(m1.pos+vector);
            m2.setRotation(m1.rot+vector2);
        }
    }
}


