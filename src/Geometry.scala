package AngryPigs

import org.lwjgl.opengl.GL11
import org.lwjgl.util.glu.GLU

class Vec3(var x:Float, var y:Float, var z:Float) {
    def this() = this(0f,0f,0f);
    
    def getPoints = List(x,y,z);
    def setPoints(v:Vec3):Vec3 = { this.x=v.x; this.y=v.y; this.z=v.z; this }
    def setPoints(p:List[Float]):Vec3 = setPoints(new Vec3(p(0),p(1),p(2)))
    def setPoints(x:Float,y:Float,z:Float):Vec3 = setPoints(new Vec3(x,y,z))
    
    override def clone = new Vec3(x,y,z);
    def map(f:Float=>Float):Vec3 = { setPoints(getPoints.map(f)); this }
    def applyVector(v:Vec3, multi:Float=1):Vec3 = setPoints(this+(v*multi))
    
    def +(v:Vec3):Vec3 = new Vec3(x+v.x, y+v.y, z+v.z)
    def -(v:Vec3):Vec3 = new Vec3(x-v.x, y-v.y, z-v.z)
    def +=(v:Vec3) = applyVector(v, +1)
    def -=(v:Vec3) = applyVector(v, -1)
    def *(f:Float):Vec3 = this.clone.map(_ * f)
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
        if(v.x > x) x = v.x;
        if(v.y > y) y = v.y;
        if(v.z > z) z = v.z;
    }
    
    // clamp values to some value(e.g. world size)
    private def clamp(p:Float,clamp:Float):Float = if(clamp!=0 && math.abs(p) > clamp) clamp*(p/math.abs(p)) else p
    def clamp(c:Float):Vec3 = this.map(clamp(_,c));
    def clamp(cx:Float,cy:Float,cz:Float):Vec3 = setPoints(clamp(x, cx), clamp(y, cy),clamp(z, cz))

    override def toString = "%.2f, %.2f, %.2f".format(x,y,z);
}
// @ a lot of these classes could be traits i think... google mixin scala
// also, is this nice in any way? :) (SomeModel with Vector seems nice)
// also traits vs abstract classes vs java interfaces
// also i'm on a train with no internets
// also there seems to be no way to add traits inline as i had hoped... bummer.
trait Vector {
    var vector = new Vec3;
    def setVector(x:Float,y:Float,z:Float) = { vector = new Vec3(x,y,z); }
}
trait Vector2 {
    var vector2 = new Vec3;
    def setVector2(x:Float,y:Float,z:Float) = { vector2 = new Vec3(x,y,z); }
}
trait Points {
    var points:Array[Vec3]=null;
}
//inner list= (pos, vec)
trait Lines {
    var lines:List[List[Vec3]]=null;
}

class Quad(var p1:Vec3, var p2:Vec3, var p3:Vec3, var p4:Vec3) {
    def getPoints = List(p1,p2,p3,p4);
    
    def foreach(f:Vec3=>Unit) = getPoints.foreach(f);
    def map(f:Vec3=>Unit) = foreach(f);
}

abstract class BasicModel {
    var (pos,rot,scale) = (new Vec3, new Vec3, new Vec3(1f,1f,1f));

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
        if(rot.x!=0) GL11.glRotatef(rot.x, 1, 0, 0);        
        if(rot.y!=0) GL11.glRotatef(rot.y, 0, 1, 0);
        if(rot.z!=0) GL11.glRotatef(rot.z, 0, 0, 1);
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
class DisplayModel(var renderfunc:()=>Unit) extends BasicModel with Vector {
    def this() = this(()=>null)
    var compiled = false;
    
    var displayList:Int = -1;
    
    def compile() {
        displayList = GL11.glGenLists(1);
        GL11.glNewList(displayList,GL11.GL_COMPILE);
        renderfunc();
        GL11.glEndList;
        compiled = true;
    }
    override def clone:DisplayModel = {
        val res = new DisplayModel(this.renderfunc)
        res.pos = this.pos.clone
        res.rot = this.rot.clone
        res.scale = this.scale.clone
        res
    }
    
    override def render() {
        GL11.glPushMatrix;
        
        if(displayList == -1)
            renderfunc;
        else
            GL11.glCallList(displayList);
        
        GL11.glPopMatrix;
    }    
}

class GeneratorModel(var generator:()=>Object, draw:Object=>Unit) extends DisplayModel {
    var data:Object = generator();
    renderfunc = ()=>{draw(data);()}
    
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
/*
class Branch {
    def this(parent:Branch, diffV:Vec3 = null, rootV: Vec3 = null) {
        this();
        rootVec = rootV;
        diffVec = diffV;
        setParent(parent);
    }
    
    import scala.collection.mutable.HashSet;
    var parent: Branch = null;
    var children = new HashSet[Branch];
    
    def setParent(b:Branch) {
        this.parent = b;
        b.children += this
    }
    def addChild(b:Branch) {
        b.setParent(this);
    }
    
    var hasLeaf:Boolean = false;
    var diffVec:Vec3 = null;
    var rootVec:Vec3 = null;
    def destVec:Vec3 = rootVec + diffVec;
    
    def getTreeBox():List[Vec3] = {
        var max = new Vec3;
        var min = new Vec3;
        
        max.takeMax(rootVec)
        max.takeMax(destVec)
        min.takeMin(rootVec)
        min.takeMin(destVec)
        
        for(child <- children) {
            var maxmin = child.getTreeBox();
            max.takeMax(maxmin(0));
            min.takeMin(maxmin(1));
        }
        
        List(max, min);
    }
    
    def setDepths(start:Int) {
        depth = start;
        children.map(_.setDepths(start+1));        
    }
    
    // OH THE HUGE MANATEEE!
    def traverseTree(data:Object, branchFunc:(Array[Float], Array[Float], Int)=>Array[Float]):Branch {
        import java.util.{List=>JavaList}
        
        var root = new Branch();
        
        def isJavaList(a:Object):Boolean = a.isInstanceOf[JavaList[Object]]
        def asArray(a:Object):Array[Object] = a.asInstanceOf[JavaList[Object]].toArray;
        def asFloatArray(a:Array[Object]):Array[Float] = a.toList.map(
                (a)=>{
                    if(a.isInstanceOf[java.lang.Double])
                        a.asInstanceOf[java.lang.Double].floatValue();
                    else
                        a.asInstanceOf[Float]
                }
            ).toArray

        var depth=0;
        var currBranch = root;
        def traverse(vec:Array[Float], data:Array[Object], branch:Branch):Array[Float] = {
            if(data.length==4 && !isJavaList(data(0))) {
                val vector = asFloatArray(data);
                
                currBranch.diffVec = new Vec3(vector(0), vector(1), vector(2));
                
                ((for(i <- 0 to 3) yield if(i==3) 1f else vec(i)*vec(3) + vector(i)*vector(3)).toArray[Float]);
            } else {
                if(data.length==1)
                    traverse(vec, asArray(data(0)))
                else if(!isJavaList(asArray(data(1)).apply(0))) {
                    depth += 1;
                    var currParent = currBranch;
                    for(i <- 0 until data.length) {
                        currBranch = new Branch(currParent, rootVec = new Vec3(vec(0), vec(1), vec(2)))
                        traverse(vec, asArray(data(i)))                        
                    }
                    depth -= 1;
                } else {
                    depth += 1;
                    var currParent = currBranch;
                    var parentVec = traverse(vec, asArray(data(0))
                    for(i <- 1 until data.length) {
                        currBranch = new Branch(currParent, rootVec = new Vec3(vec(0), vec(1), vec(2)))
                        traverse(), asArray(data(i)))
                    }
                    depth -= 1;
                }

                // gotta return something...
                return vec;
            }
        }
        
        traverse(Array[Float](0,0,0,1), asArray(data))
    }    
}
*/
class Camera extends BasicModel {
    // default projection 
    var perspective = false
    var (near,far) = (1f,30f) // near, far clipping plane
    var (fov,aspectRatio) = (45f,4/3f) // perspective stuff
    var (minX,minY,maxX,maxY) = (-1f,-1f, 1f, 1f) // ortho stuff

    // set a perspective projection
    def setPerspective(fv:Float, ar:Float, n:Float, f:Float) {
        perspective=true
        fov=fv
        aspectRatio=ar
        near=n
        far=f
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
    }
    
    private var lookAtV = new Vec3
    def lookAt(v:Vec3) = lookAtV = v.clone;
    def lookAt(m:BasicModel) = lookAtV = m.pos.clone;
    
    override def render {
        // setup projection matrix stack
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity();
        if(perspective) {
            // perspective projection
            GLU.gluPerspective(fov,aspectRatio, near,far);
        } else {
            // orthographic projection 
            GL11.glOrtho(minX,maxX, minY,maxY, near,far);
        }

        // model view stack 
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity();
        
        GLU.gluLookAt(pos.x,pos.y,pos.z,             // camera position
                      lookAtV.x,lookAtV.y,lookAtV.z, // look-at vector
                      0,1,0)                         // up vector 
    }
}

class ModelLink(var m1:BasicModel, var m2:BasicModel) extends Vector with Vector2 {
    def this(m1:BasicModel,m2:BasicModel,vector:Vec3, vector2:Vec3) {
        this(m1,m2)
        this.vector = vector;
        this.vector2 = vector2;
    }
    def this(m1:BasicModel,m2:BasicModel,vector:Vec3) {
        this(m1,m2, vector,new Vec3)
    }

    private var linked = true;
    def isLinked = linked;
    def breakLink { linked = false }
    def forgeLink { linked = true }
    
    def applyLink {
        if(linked) {
            m2.setPosition(m1.pos+vector);
            m2.setRotation(m1.rot+vector2);
        }
    }
}


