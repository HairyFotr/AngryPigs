package AngryPigs

import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import org.lwjgl.util.vector._
import scala.util.Random
import scala.collection.mutable._
import clojure.lang._
import clojure.core._
import java.nio._

// TODO:
// search for @ <task>

class clojureWrap(ns:String,obj:String) {
    RT.loadResourceScript(obj+".clj");  
    
    //@morda bi se dal s parcialno funkcijo obj/"func"(args) in bi invoke prevzel vse (args)
    def /(func:String, a:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object]))
    def /(func:String, a:Any, b:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object]))
    def /(func:String, a:Any, b:Any, c:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object]))
    def /(func:String, a:Any, b:Any, c:Any, d:Any) = (RT.`var`(ns+"."+obj, func).invoke(a.asInstanceOf[Object], b.asInstanceOf[Object], c.asInstanceOf[Object], d.asInstanceOf[Object]))
}

object TimeLock {
    private var lock = false;
    def isLocked:Boolean = {
        if(lock) {
            if(milliTime-lockTime > lockDuration) {
                lock = false;
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }
    
    private def milliTime = System.nanoTime()/1000000L
    
    private var lockTime = milliTime;
    private var lockDuration = 0L;
    def lockIt(ms:Int) = {
        lockTime = milliTime;
        lockDuration = ms;
        lock = true
    }
}
object Quadrics {
    val sphere = new Sphere;
    val cylinder = new Cylinder;
    val disk = new Disk;
}

object Game {

    var isRunning = false; // is main loop running
    //val acceptModes
    var (winWidth, winHeight)=(3000,3000); // window size
    var renderTime=0f;
    val cam = new Camera;
    val rand = new Random;
    val genTree = new clojureWrap("AngryPigs", "gen-tree");
    //val helloworld = new clojureWrap("AngryPigs", "helloworld");
    
    /**
     * Initializes display and enters main loop
     */
    def main(Args:Array[String]) {
        //var h = new AngryPig.helloworld2;
        //printlnset(h.neki(6));
        
        //println(genTree/("give-me-tree", new Array[Int](1,2,3,5), 0))
        //val i = new clojure.lang.Sequence(5, 6);
        //val i = new clojure.core.seq(List(5,6));
        
        try {
          initDisplay;
        } catch {
            case e:Exception => {
              print("Can't open display. "+e.getMessage);
              exit(1);
            }
        }

        // enter loop
        isRunning = true;
        mainLoop;
        
        // cleanup
        Display.destroy;
    }

    def initDisplay {
        var bestMode:DisplayMode = null;
        val modes:Array[DisplayMode] = Display.getAvailableDisplayModes;
        // Get best mode
        for(mode <- modes)
            if((mode.getWidth <= winWidth && mode.getHeight <= winHeight && mode.getFrequency <= 100)
                &&(bestMode == null
                   ||(mode.getWidth >= bestMode.getWidth && mode.getHeight >= bestMode.getHeight
                      && mode.getBitsPerPixel >= bestMode.getBitsPerPixel
                      && mode.getFrequency >= bestMode.getFrequency)))
                bestMode = mode;
        
        Display.setDisplayMode(bestMode);
        winWidth = bestMode.getWidth;
        winHeight = bestMode.getHeight;
        
        println("Display: "+bestMode.getWidth+"x"+bestMode.getHeight+"@"+bestMode.getFrequency+"Hz, "+bestMode.getBitsPerPixel+"bit")
        
        try {
            // FSAA
            Display.create(new PixelFormat(8, 16, 0, 1));
        } catch {
            case _ =>
                // No FSAA
                Display.create;
        }
        Display.setTitle("Angry Pigs");
        Display.setVSyncEnabled(true);
    }

    def now = System.nanoTime();
    // used for frame independant movement
    var frameTime = now;
    
    /**
     * Main loop: renders and processes input events
     */
    def mainLoop { 
        //loadModels; // load models
        makeModels; // make generative models
        setupView;  // setup camera and lights
    
        frameTime = now;
        // @that is one ugly FPS counter :)
        var secondTimer = now;
        var frameCounter = 0;
        val E10 = 10000000000L;
        while(isRunning) {
            resetView;      // clear view and reset transformations
            renderFrame;    // draw stuff
            // @menda se da sproti/bolš gledat input
            processInput;   // process input events 
            Display.update; // update window contents and process input messages
            frameCounter += 1;

            renderTime = (now-frameTime)/frameIndepRatio;
            frameTime = now;
            //while(now-frameTime<25000000) Thread.sleep(1)
            
            //gl error
            val errCode = GL11.glGetError;
            if (errCode != GL11.GL_NO_ERROR) 
                println(opengl.Util.translateGLErrorString(errCode));

            if(now-secondTimer > E10) {
                secondTimer = now;
                // print fps
                println("FPS: "+frameCounter/10);
                frameCounter = 0;
            }
        }
    }
    
    //models
    var terrain:QuadPatch=null;
    var skybox:DisplayModel=null;
    var coordsys:DisplayModel=null;
    var pig:DisplayModel=null;
    var catapult:DisplayModel=null;
    var tree:DisplayModel=null;
    var pigcatapultLink:ModelLink=null;
    var campigLink:ModelLink=null;
    //size of world
    val worldSize = 400;
    val gravity = new Vec3(0f,-0.5f,0f);
    
    var fattrees = true;
    
    // @would it pay-off to make model generation lazy and generate them on the fly?
    // @infinite terrain patches and stuff
    def makeModels {
        // terrain
        val detail=40;
        val height=0.3f;
        
        def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
        val p = (for(i <- 0 to detail; j <- 0 to detail) yield getTerrainPoint(i,j)).toArray;        
        terrain = new QuadPatch(p, detail+1);
        terrain.setPosition(-worldSize,-worldSize,-worldSize);
        terrain.setScale(worldSize*2, 5, worldSize*2);
        
        // coordinate system
        coordsys = new DisplayModel(Unit=>{
            GL11.glBegin(GL11.GL_LINES);
                GL11.glColor3f(1,0,0);
                GL11.glVertex3f(0,0,0);
                GL11.glVertex3f(1,0,0);
                
                GL11.glColor3f(0,1,0);
                GL11.glVertex3f(0,0,0);
                GL11.glVertex3f(0,1,0);
                
                GL11.glColor3f(0,0,1);
                GL11.glVertex3f(0,0,0);
                GL11.glVertex3f(0,0,1);
            GL11.glEnd;//*/
        });
        coordsys.setScale(worldSize,worldSize,worldSize);

        // sky-box
        skybox = new DisplayModel(Unit=>{
            GL11.glBegin(GL11.GL_QUADS);
                GL11.glColor3f(0.5f,0.5f,0.5f);
                // top
                //GL11.glColor3f(0f,1f,0f); // green
                GL11.glVertex3f( 2f, 1f,-2f);
                GL11.glVertex3f(-2f, 1f,-2f);
                GL11.glVertex3f(-2f, 1f, 2f);
                GL11.glVertex3f( 2f, 1f, 2f);
                // bottom 
                /*GL11.glColor3f(1f,0.5f,0f);    // orange
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f( 1f,-1f,-1f);*/
                // front
                //GL11.glColor3f(1f,0f,0f); // red 
                GL11.glVertex3f( 2f, 1f, 2f);
                GL11.glVertex3f(-2f, 1f, 2f); 
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                // back
                //GL11.glColor3f(1f,1f,0f); // yellow
                GL11.glVertex3f( 1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-2f, 1f,-2f);
                GL11.glVertex3f( 2f, 1f,-2f);
                // left
                //GL11.glColor3f(0f,0f,1f); // blue
                GL11.glVertex3f(-2f, 1f, 2f);
                GL11.glVertex3f(-2f, 1f,-2f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                // right
                //GL11.glColor3f(1f,0f,1f); // violet
                GL11.glVertex3f( 2f, 1f,-2f);
                GL11.glVertex3f( 2f, 1f, 2f);
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f,-1f);
            GL11.glEnd;
        });
        skybox.setPosition(0,0,0);
        skybox.setScale(worldSize,worldSize,worldSize);//*/

        // pig
        pig = new DisplayModel(Unit=>{
            //body
            GL11.glColor3f(0.3f,0.8f,0.3f);
            GL11.glPushMatrix;
            {
                GL11.glScalef(0.95f,1,1.05f);
                Quadrics.sphere.draw(2,22,22);
            }
            GL11.glPopMatrix
            //ears
            GL11.glColor3f(0.4f,0.9f,0.4f);
            GL11.glPushMatrix;
            {
                val x = 0.9f;
                GL11.glRotatef(180,0,1,0)
                GL11.glTranslatef(x,1.7f,-0.7f);
                Quadrics.disk.draw(0,0.35f, 15,1);
                GL11.glTranslatef(-2*x,0,0);
                Quadrics.disk.draw(0,0.35f, 15,1);
            }
            GL11.glPopMatrix
            //nose            
            GL11.glColor3f(0.4f,1f,0.4f);
            GL11.glPushMatrix;
            {
                GL11.glScalef(1,1,1);
                //GL11.glRotatef(90, 0,1,0)
                GL11.glTranslatef(0,0.4f,1.4f);
                val size=0.7f
                Quadrics.cylinder.draw(size,size, 1, 20,1);
                GL11.glTranslatef(0,0f,1f);
                Quadrics.disk.draw(0,size, 20,1);
            }
            GL11.glPopMatrix
            //eyes
            GL11.glPushMatrix;
            {
                val x = 1.2f;
                def eye = {
                    GL11.glPushMatrix;
                    GL11.glColor3f(0.8f,0.8f,0.8f);
                    Quadrics.sphere.draw(0.5f,10,10);
                    val z = 0.35f;
                    GL11.glTranslatef(0,0,z);
                    GL11.glColor3f(0.1f,0.1f,0.1f);
                    Quadrics.sphere.draw(0.25f,10,10);
                    GL11.glPopMatrix
                }
                GL11.glTranslatef(x,0.6f,1.2f);
                eye;
                GL11.glTranslatef(-2*x,0,0);
                eye;
            }
            GL11.glPopMatrix
        });
        pig.setPosition(0,-worldSize+7f,-worldSize/2+25);
        //pig.setScale(worldSize/,worldSize,worldSize);//*/

        catapult = new DisplayModel(Unit=>{
            val scale = new Vec3(4f,1f,6.5f)
            GL11.glPushMatrix;
            GL11.glScalef(scale.x,scale.y,scale.z);
            GL11.glColor3f(0.8f,0.3f,0f);
            GL11.glBegin(GL11.GL_QUADS);                
                // top
                GL11.glNormal3f( 0f, 1f, 0f);
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                // bottom 
                GL11.glNormal3f( 0f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f( 1f,-1f,-1f);
                // Front
                GL11.glNormal3f( 0f, 0f, 1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f, 1f); 
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                // back
                GL11.glNormal3f( 0f, 0f,-1f);
                GL11.glVertex3f( 1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f,-1f);
                // left
                GL11.glNormal3f(-1f, 0f, 0f);
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                // right
                GL11.glNormal3f( 1f, 0f, 0f);
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f,-1f);
            GL11.glEnd;
            GL11.glPopMatrix;

            def wheel = {
                GL11.glRotatef(90, 0,1,0)
                Quadrics.cylinder.draw(1f,1f, scale.x*2+2, 25,1);
                Quadrics.disk.draw(0,1,20,1);
                GL11.glTranslatef(0,0,scale.x*2+2);
                Quadrics.disk.draw(0,1,20,1);
            }
            // Front wheel
            GL11.glPushMatrix;
                GL11.glTranslatef(-scale.x-1,-1,scale.z-2f)
                wheel
            GL11.glPopMatrix;
            // Back wheel
            GL11.glPushMatrix;
                GL11.glTranslatef(-scale.x-1,-1,-scale.z+2f)
                wheel
            GL11.glPopMatrix;
        });
        catapult.setPosition(0,-worldSize+2.5f,-worldSize/2+25);
        
        pigcatapultLink = new ModelLink(catapult, pig, new Vec3(0f,2.5f,0f));
        campigLink = new ModelLink(pig, cam, new Vec3(0f,7,-50), new Vec3(0,0,0));
        
        
        tree = new DisplayModel(Unit=>{
            var depth=0;
            def drawTree(a:Array[Object], v:Array[Float]):Array[Float] = {
                if(a.length==4 && !a(0).isInstanceOf[java.util.List[Object]]) {
                    val vector = a.toList.map(_.toString.toFloat).toArray;
                    
                    val vec = if(v == null) Array[Float](0f,0f,0f,1f) else v
                    
                    if(fattrees) {
                        GL11.glPushMatrix
                        val vecA = new Vec3(vec(0)*vec(3),
                                            vec(1)*vec(3),
                                            vec(2)*vec(3))
                                            
                        val vecB = new Vec3(vec(0)*vec(3) + vector(0)*vector(3),
                                            vec(1)*vec(3) + vector(1)*vector(3),
                                            vec(2)*vec(3) + vector(2)*vector(3))
                        val z = new Vec3(0,0,1)
                        val p = vecA - vecB
                        val t = z X p;

                        val angle = 180f/math.Pi * math.acos((z dot p)/p.length);

                        GL11.glTranslatef(vecB.x,vecB.y,vecB.z);
                        GL11.glRotatef(angle.toFloat,t.x,t.y,t.z);
                        
                        Quadrics.cylinder.draw(0.2f/(depth),0.3f/(depth), if(depth==1) vector(1)*vector(3) else vector(3), 25,1);
                        GL11.glPopMatrix
                    } else {
                        GL11.glBegin(GL11.GL_LINES)
                        GL11.glVertex3f(vec(0)*vec(3),
                                        vec(1)*vec(3),
                                        vec(2)*vec(3));
                        
                        GL11.glVertex3f(vec(0)*vec(3) + vector(0)*vector(3),
                                        vec(1)*vec(3) + vector(1)*vector(3),
                                        vec(2)*vec(3) + vector(2)*vector(3))
                        GL11.glEnd
                    }

                    //vector.toList.foreach(println)
                    println(depth)
                    
                    return (for(i <- 0 to 3) yield if(i==3) 1f else vec(i)*vec(3) + vector(i)*vector(3)).toArray
                } else {
                    var finalBranch = false;
                    if(a.length>=2) {
                        val aa = (a(1).asInstanceOf[java.util.List[Object]].toArray)
                        if(!aa(0).isInstanceOf[java.util.List[Object]]) {
                            finalBranch = true;
                        }
                    }
                    
                    if(finalBranch) {
                        for(i <- 0 until a.length) {
                            depth += 1;
                            drawTree(a(i).asInstanceOf[java.util.List[Object]].toArray, v)
                            depth -= 1;
                        }
                    } else if(a.length>=2) {
                        depth += 1;
                            val vector = drawTree(a(0).asInstanceOf[java.util.List[Object]].toArray, v);
                            
                            drawTree(a(1).asInstanceOf[java.util.List[Object]].toArray, vector)
                        depth -= 1;
                    }

                    // gotta return something...
                    return v;
                }
            }
            
            GL11.glColor3f(1,1,1);
            GL11.glColor3f(0.7f,0.2f,0f);

            val tree = (genTree/("give-me-tree", 0.0f, 2f, 0.0f, 5f)).asInstanceOf[java.util.List[Object]].toArray;            
            drawTree(tree, null);
        });
        tree.setPosition(0,-worldSize+2.5f,-worldSize/2+30);
    }

    def allocFloats(floatarray:Array[Float]):FloatBuffer = {
        val fb:FloatBuffer = ByteBuffer.allocateDirect(floatarray.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
        fb.put(floatarray).asInstanceOf[FloatBuffer].flip();
        return fb;
    }
      /**
     * Initial setup of projection of the scene onto screen, lights etc.
     */
    def setupView {
        GL11.glEnable(GL11.GL_DEPTH_TEST); // enable depth buffer (off by default)
        //GL11.glEnable(GL11.GL_CULL_FACE);  // enable culling of back sides of polygons
        //GL11.glCullFace(GL11.GL_BACK);
      
        // smooth shading - Gouraud
        GL11.glShadeModel(GL11.GL_SMOOTH);
        //GL11.glShadeModel(GL11.GL_FLAT);

        // lights
        GL11.glEnable(GL11.GL_LIGHTING);
        GL11.glEnable(GL11.GL_LIGHT0);

        GL11.glLight(GL11.GL_LIGHT0, GL11.GL_AMBIENT, allocFloats(Array[Float](0.3f, 0.3f, 0.3f, 0.0f)));
        GL11.glLight(GL11.GL_LIGHT0, GL11.GL_DIFFUSE, allocFloats(Array[Float](0.7f, 0.7f, 0.7f, 0.0f)));
        GL11.glLightf(GL11.GL_LIGHT0, GL11.GL_LINEAR_ATTENUATION, 20f);
        GL11.glLight(GL11.GL_LIGHT0, GL11.GL_POSITION, allocFloats(Array[Float](0f, 0f, 10f, 0f)));
        GL11.glEnable(GL11.GL_COLOR_MATERIAL)
        GL11.glMaterial(GL11.GL_FRONT, GL11.GL_AMBIENT_AND_DIFFUSE, allocFloats(Array[Float](0.9f, 0.9f, 0.9f, 0f)));
        GL11.glColorMaterial(GL11.GL_FRONT, GL11.GL_AMBIENT_AND_DIFFUSE );
        
        GL11.glViewport(0,0, winWidth,winHeight); // mapping from normalized to window coordinates
       
        GL11.glHint(GL11.GL_PERSPECTIVE_CORRECTION_HINT, GL11.GL_NICEST);
        cam.setPerspective(50, winWidth/winHeight.toFloat, 1f, 600f);
        cam.setPosition(0,worldSize-5,-worldSize+5);
        cam.setRotation(0,0,0);
    }
  
    /**
    * Resets the view of current frame
    */
    def resetView {
        // clear color and depth buffer
        GL11.glClearColor(0.3f,0.6f,0.8f,1f);
        GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity;
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity;
    }
  
    var frameIndepRatio = (20000000f);
    var treeView = false;

    def moveObj = if(treeView) tree else { if(pigcatapultLink.isLinked) catapult else pig };
        
    /**
    * Renders current frame
    */
    def renderFrame {       
        val toRender = List(
            //cam,
            //coordsys,
            terrain,
            //skybox,
            pig,
            catapult,
            tree
        )
        
        // move pig or catapult
        moveObj.vector.z -= 0.05f*moveObj.vector.z*renderTime;
        moveObj.vector.clamp3(0,0,8);

        pig.vector.applyVector(gravity*renderTime);

        val actualMoveVector = 
            new Vec3(
                math.sin(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z,
                moveObj.vector.y,
                math.cos(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z
            )
        moveObj.pos.applyVector(actualMoveVector*renderTime);
        
        moveObj.pos.clamp(worldSize-2.5f);
        
        pigcatapultLink.applyLink;
        campigLink.applyLink;
        //cam.pos.clamp(worldSize-5);
                
        //set projection and reset modelview
        //look at this pig... look!
        cam.lookAt(moveObj)        
        cam.render

        toRender.foreach(
            (model:BasicModel)=>{
                GL11.glPushMatrix;
                model.doTranslation
                model.doRotation
                model.doScalation

                model.render
                GL11.glPopMatrix;
            }
        )
    }
    
    def processInput {
        if(Display.isCloseRequested || Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) isRunning = false;
        
        if(Keyboard.isKeyDown(Keyboard.KEY_T) && !TimeLock.isLocked) {
            treeView = !treeView
            TimeLock.lockIt(1000);
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_1)) { fattrees = false; tree.compile }
        if(Keyboard.isKeyDown(Keyboard.KEY_2)) { fattrees = true; tree.compile }

        
        val keymove = 0.7f*renderTime;
        
        if(campigLink.isLinked) {
            if(Keyboard.isKeyDown(Keyboard.KEY_Q)) campigLink.vector2.x+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_E)) campigLink.vector2.x-=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_A)) campigLink.vector2.y+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_D)) campigLink.vector2.y-=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_Y)) campigLink.vector2.z+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_C)) campigLink.vector2.z-=keymove;

            if(Keyboard.isKeyDown(Keyboard.KEY_W)) campigLink.vector.x+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_R)) campigLink.vector.x-=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_S)) campigLink.vector.y+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_F)) campigLink.vector.y-=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_X)) campigLink.vector.z+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_V)) campigLink.vector.z-=keymove;
        } else {            
            if(Keyboard.isKeyDown(Keyboard.KEY_Q)) cam.rot.x+=keymove*2000;
            if(Keyboard.isKeyDown(Keyboard.KEY_E)) cam.rot.x-=keymove*2000;
            if(Keyboard.isKeyDown(Keyboard.KEY_A)) cam.rot.y+=keymove*2000;
            if(Keyboard.isKeyDown(Keyboard.KEY_D)) cam.rot.y-=keymove*2000;
            if(Keyboard.isKeyDown(Keyboard.KEY_Y)) cam.rot.z+=keymove*2000;
            if(Keyboard.isKeyDown(Keyboard.KEY_C)) cam.rot.z-=keymove*2000;

            if(Keyboard.isKeyDown(Keyboard.KEY_W)) cam.pos.x+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_R)) cam.pos.x-=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_S)) cam.pos.y+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_F)) cam.pos.y-=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_X)) cam.pos.z+=keymove;
            if(Keyboard.isKeyDown(Keyboard.KEY_V)) cam.pos.z-=keymove;
        }

        //if(Keyboard.isKeyDown(Keyboard.KEY_LEFT))  moveObj.vector.x+=keymove/3f;
        //if(Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) moveObj.vector.x-=keymove/3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_LEFT))  moveObj.rot.y+=keymove*3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) moveObj.rot.y-=keymove*3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_UP))    moveObj.vector.z+=keymove/5f;
        if(Keyboard.isKeyDown(Keyboard.KEY_DOWN))  moveObj.vector.z-=keymove/5f;
        
        // clamps x+z vector... enable after you know when the pig is floating :)
        /*if(!pigcatapultLink.isLinked) {
            val vclamp=0.8f;
            def sgn(n:Float) = n/math.abs(n);
            if(math.abs(pig.vector.x) > vclamp) pig.vector.x = vclamp*sgn(pig.vector.x);
            if(math.abs(pig.vector.z) > vclamp) pig.vector.z = vclamp*sgn(pig.vector.z)
            
            if(math.abs(pig.vector.x)+math.abs(pig.vector.z) > vclamp) {
                //if(math.abs(pig.vector.x) > math.abs(pig.vector.y)) {
                    //x+y>vclamp ... x*r+z*r = vclamp ... r = vclamp/x+z
                    val r = vclamp/(math.abs(pig.vector.x)+math.abs(pig.vector.z))
                    pig.vector.x *= r
                    pig.vector.z *= r
                //
            }
        }//*/

        if(Keyboard.isKeyDown(Keyboard.KEY_SPACE) && !TimeLock.isLocked){
            if(pigcatapultLink.isLinked) {
                pigcatapultLink.breakLink;
                println("pig-catapult Link Broken")
                campigLink.breakLink
                println("cam-pig Link Broken")
                pig.vector.y=5f;
                pig.vector.z=8f;
            } else {
                pig.vector.y=2f;
                pig.vector.z=2f;
            }
            TimeLock.lockIt(1000);
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_LCONTROL) && !pigcatapultLink.isLinked) {
            pigcatapultLink.forgeLink;
            println("pig-catapult Link Forged")
            campigLink.forgeLink
            println("cam-pig Link Forged")
        }

        if(Keyboard.isKeyDown(Keyboard.KEY_P)) {
            println(cam.toString);
            println(pig.toString);
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_8) && campigLink.isLinked) {
            campigLink.breakLink
            println("cam-pig Link Broken")
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_9) && !campigLink.isLinked) {
            campigLink.forgeLink
            println("cam-pig Link Forged")
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_0)) {
            cam.setPosition(0,worldSize-5,-worldSize+5);
            cam.setRotation(0,180,0);
            println("cam Reset")
        }
    }    
}



