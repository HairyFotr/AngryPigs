package AngryPigs

import org.lwjgl.opengl.{Display,PixelFormat,DisplayMode,Util}
import org.lwjgl.input.Keyboard
import scala.collection.mutable.ListBuffer
import java.util.{List => JavaList}
import java.nio._
import scala.actors.Futures._
import scala.actors.Future

// TODO:
// search for @ <task>
// longterm: 
/// decouple physics and render
/// infinite terrain patches and stuff
object Game {
    import Global._
    import org.lwjgl.opengl.GL11._

    var isRunning = false; // is main loop running
    var (winWidth, winHeight)=(3000,3000); // window size
    var renderTime=0f;
    
    val timeLock = new TimeLock;
    val pauseLock = new TimeLock;
    
    /**
     * Initializes display and enters main loop
     */
    def main(Args:Array[String]) {
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
        mainLoop();
        
        // cleanup
        Display.destroy();
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
            // No FSAA            
            case _ =>
                Display.create;
        }

        Display.setTitle("Angry Pigs");
        Display.setVSyncEnabled(true);
    }

    def now = System.nanoTime();
    // used for frame independant movement
    var frameTime = now;
    
    
    def decreaseDetail() = {
        if(settings.get[Int]("maxdepth")>4) settings += "maxdepth" -> 4;
        settings += "graphics" -> (settings.get[Int]("graphics")-1);
        println("decreased graphic detail to "+settings.get[Int]("graphics"));
        models().foreach((model)=> {                        
            tasks() += (()=>model.compile());
            if(model.compileCache.size > 5 && model.compileCache.size <= 7) tasks() += (()=>model.reset);
        })
    }
    def increaseDetail() = {
        if(settings.get[Int]("maxdepth")<6) settings += "maxdepth" -> (settings.get[Int]("maxdepth")+1);
        settings += "graphics" -> (settings.get[Int]("graphics")+1);
        println("increased graphic detail to "+settings.get[Int]("graphics"));
        models().foreach((model)=> {
            tasks() += (()=>model.compile());
            if(model.compileCache.size > 3 && model.compileCache.size <= 7) tasks() += (()=>model.reset);
        })
    }
    
    /**
     * Main loop: renders and processes input events
     */
    def mainLoop() { 
        makeModels; // make generative models
        setupView;  // setup camera and lights
    
        // FPS counter
        var frameCounter = 0
        val second = 1000000000L
        val FPSseconds = 3
        var FPStimer = now
        frameTime = now
        while(isRunning) {
            processInput // process keyboard input
            if(pause) Thread.sleep(50)
            
            resetView;      // clear view and reset transformations
            renderFrame;    // draw stuff
            Display.update; // update window contents and process input messages
            frameCounter += 1;

            if(now-FPStimer > second*FPSseconds) {
                val FPS = frameCounter/FPSseconds.toFloat;
                
                // increase or decrease graphics detail
                if(FPS < 16 && settings.get[Int]("graphics") > 1) decreaseDetail();
                if(FPS > 45 && settings.get[Int]("graphics") < 7) increaseDetail();
            
                if(tasks().length < 10) {
                    models().foreach((model)=> {
                        if(model.compileCache.size > 7) tasks() += (()=>model.reset);
                    })
                }
                
                println("FPS: "+FPS);
                println("Tasks: "+tasks.length);
                print("render: "+renderTimes)
                print(" -- physics: "+physicsTimes)
                print(" -- worker: "+workerTimes)
                println(" -- full: "+fullTimes)

                frameCounter = 0;
                FPStimer = now;
            }

            renderTime = (now-frameTime)/frameIndepRatio;
            frameTime = now;
        }
    }
    
    //models
    val cam = new Camera;
    var terrain:GeneratorModel with Properties=null;
    var skybox:DisplayModel=null;
    var coordsys:DisplayModel=null;
    var pig:GeneratorModel=null;
    var catapult:DisplayModel=null;
    var trees = new ListBuffer[GeneratorModel];
    var pigcatapultLink:ModelLink=null;
    //var campigLink:ModelLink=null;
    var dropBranches = new ListBuffer[GeneratorModel];
    var trails = new ListBuffer[TrailModel];
    var futureTrees = new ListBuffer[Future[GeneratorModel]];

    def models():scala.collection.Traversable[DisplayModel] = {
        List(pig, catapult, terrain) ++ trees ++ dropBranches ++ trails
    }
    
    //size of world
    val worldSize = 400;
    val gravity = new Vec3(0f,-0.5f,0f);
       
    object Tree {
        def giveMeTree:()=>Object = ()=>{
            // OH THE HUGE MANATEEE!
            import java.util.{List=>JavaList}
            
            def isJavaList(a:Object):Boolean = a.isInstanceOf[JavaList[Object]]
            def asArray(a:Object):Array[Object] = a.asInstanceOf[JavaList[Object]].toArray;
            def asFloatArray(a:Array[Object]):Array[Float] = {a.toList.map((a)=>{
                if(a.isInstanceOf[java.lang.Double])
                    a.asInstanceOf[java.lang.Double].floatValue();
                else
                    a.asInstanceOf[Float]
            }).toArray}

            def traverse(data:Array[Object], parent:Branch=null):Branch = {
                if(data.length==1) { // unpack thingy ... ((...))
                    traverse(asArray(data(0)), parent)
                } else if(data.length==4 && !isJavaList(data(0))) { // leaves ... (node)
                    val vector = asFloatArray(data);
                    var res = new Branch(parent);
                    if(parent!=null) res.rootVec = parent.rootVec+parent.diffVec;
                    res.diffVec = (new Vec3(vector(0), vector(1), vector(2))) * vector(3)
                    res.properties += (if(rand.nextFloat < 0.085*res.depth) "hasLeaf" -> true else "hasLeaf" -> false);
                    res;
                } else if(!isJavaList(asArray(data(0)).apply(0)) && isJavaList(asArray(data(1)).apply(0))) { // parent & subbranches ((node) (...))
                    var newparent = traverse(asArray(data(0)), parent);
                    for(i <- 1 until data.length) traverse(asArray(data(i)), newparent);
                    newparent;
                } else { // branches ... ((...) (...) (...))
                    for(i <- 0 until data.length) traverse(asArray(data(i)), parent);
                    parent;
                }
            }
            
            var data:Object = null;
            while(data==null) try {
                data = genTree/("give-me-tree", 
                    0f+rand.nextFloat()/10-rand.nextFloat()/10, 
                    2f+rand.nextFloat()/2-rand.nextFloat()/3, 
                    0f+rand.nextFloat()/10-rand.nextFloat()/10, 
                    5f+rand.nextFloat()-rand.nextFloat()/2);
            } catch {
                case _ => {
                    //e.printStackTrace;
                    println("give-me-tree threw exception");                    
                    data = null;
                }
            }
            
            val tree = traverse(asArray(data));
            tree.properties += "treekind" -> 0;//rand.nextInt(3);
            tree.properties += "fatness" -> (0.25f+rand.nextFloat()/25f-rand.nextFloat()/25f);
            
            def generateBoxes(branch:Branch):BoundingBox = {
                var box = new BoundingBox(List(branch.rootVec, branch.destVec));
                
                for(child <- branch.children)
                    box += generateBoxes(child);
                
                branch.properties += "box" -> box;
                if(branch.children.length==0) {
                    branch.properties += "fatness" -> (0.18f-rand.nextFloat()/30f);
                } else {
                    branch.properties += "fatness" -> (0.2f-rand.nextFloat()/30f);
                }
                    
                branch.properties += "treekind" -> tree.properties.get[Int]("treekind");
                box;
            }
            generateBoxes(tree);
            
            tree;
            //seal here:P
        };
        def renderTree:Object=>Unit = (data:Object)=>data.asInstanceOf[Branch].doAll(_.render);
        def treeId:(DisplayModel,SettingMap[String])=>Int = (dmodel,properties)=>{
            val model = dmodel.asInstanceOf[GeneratorModel];
            var mid = 0;///to properties on fly
            // takes into account branch count, graphic detail and fatline setting
            model.data.asInstanceOf[Branch].doAll((branch)=>{ mid += 1 })
            mid += mid * properties.get[Int]("graphics");
            if(properties.get[Boolean]("fatlines")) mid = -mid;
            mid;
        };
        def futureTree:Future[GeneratorModel] = future {
            //println("the future is now: "+System.nanoTime()/1000000L)
            var tree = new GeneratorModel(giveMeTree, renderTree, treeId);
            //tree.setPosition(17,-worldSize+2.5f,-worldSize/2+30);
            tree.setPosition(
                (17+rand.nextFloat()*3-rand.nextFloat()*3)*rand.nextInt(7) - (17+rand.nextFloat()*3-rand.nextFloat()*3)*rand.nextInt(7),
                -worldSize,
                -worldSize/2+30 + (17+rand.nextFloat()*3-rand.nextFloat()*3)*rand.nextInt(10) - (17+rand.nextFloat()*3-rand.nextFloat()*3)*rand.nextInt(5));
            //tree.compile();
            println("the future is here: "+System.nanoTime()/1000000L); 
            tree
        }
    }

    def makeModels {
        // terrain
        val detail=30;
        val height=0.3f;
        
        def genTerrain:()=>Object = ()=>{
            def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
            val p = (for(i <- 0 to detail; j <- 0 to detail) yield getTerrainPoint(i,j)).toArray;
            p;
        }
        def drawTerrain = (data:Object)=>{
            val points = data.asInstanceOf[Array[Vec3]];
            glBegin(GL_QUADS);
            // Draw in clockwise - (00,10,11,01); must skip last point of line
            val width = math.sqrt(points.length).toInt;
            for(i <- 0 until points.length-width-1; if((i+1)%width != 0))
                List(points(i), points(i+1), points(i+width+1), points(i+width)).foreach(
                    (p:Vec3) => {
                        //glColor3f(0.2f, 0.7f+p.y/2, 0.2f);
                        glColor3f(0.2+p.y/4, 0.65f+p.y/1.5, 0.2+p.y/4);
                        glNormal3f(p.y, p.y, p.y);
                        glVertex3f(p.x, p.y, p.z);
                    }
                )
            glEnd;//*/
            ();
        }
        
        terrain = new GeneratorModel(genTerrain, drawTerrain) with Properties;
        terrain.setPosition(-worldSize,-worldSize,-worldSize);
        terrain.setScale(worldSize*2, 5, worldSize*2);
        terrain.compile();
                
        // coordinate system
        coordsys = new DisplayModel(()=>{
            glBegin(GL_LINES);
                glColor3f(1,0,0);
                glVertex3f(0,0,0);
                glVertex3f(1,0,0);
                
                glColor3f(0,1,0);
                glVertex3f(0,0,0);
                glVertex3f(0,1,0);
                
                glColor3f(0,0,1);
                glVertex3f(0,0,0);
                glVertex3f(0,0,1);
            glEnd;//*/
        });
        coordsys.setScale(worldSize,worldSize,worldSize);

        // sky-box
        skybox = new DisplayModel(()=>{
            glBegin(GL_QUADS);
                glColor3f(0.5f,0.5f,0.5f);
                // top
                //glColor3f(0f,1f,0f); // green
                glVertex3f( 2f, 1f,-2f);
                glVertex3f(-2f, 1f,-2f);
                glVertex3f(-2f, 1f, 2f);
                glVertex3f( 2f, 1f, 2f);
                // bottom 
                /*glColor3f(1f,0.5f,0f);    // orange
                glVertex3f( 1f,-1f, 1f);
                glVertex3f(-1f,-1f, 1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f( 1f,-1f,-1f);*/
                // front
                //glColor3f(1f,0f,0f); // red 
                glVertex3f( 2f, 1f, 2f);
                glVertex3f(-2f, 1f, 2f); 
                glVertex3f(-1f,-1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                // back

                //glColor3f(1f,1f,0f); // yellow
                glVertex3f( 1f,-1f,-1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-2f, 1f,-2f);
                glVertex3f( 2f, 1f,-2f);
                // left
                //glColor3f(0f,0f,1f); // blue
                glVertex3f(-2f, 1f, 2f);
                glVertex3f(-2f, 1f,-2f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-1f,-1f, 1f);
                // right
                //glColor3f(1f,0f,1f); // violet
                glVertex3f( 2f, 1f,-2f);
                glVertex3f( 2f, 1f, 2f);
                glVertex3f( 1f,-1f, 1f);
                glVertex3f( 1f,-1f,-1f);
            glEnd;
        });
        skybox.setPosition(0,0,0);
        skybox.setScale(worldSize,worldSize,worldSize);//*/

        // pig
        
        def genPig:()=>Object = ()=>{
            var pigData = new SettingMap[String];
            pigData += "Moustache.has" -> (rand.nextFloat > 0.2)
            pigData += "Moustache.which" -> (rand.nextInt(2))
            pigData += "Glasses.has" -> (rand.nextFloat > 0.2)
            pigData += "Glasses.which" -> (rand.nextInt(3))
        }
        
        def drawPig(data:Object):Unit = {
            var pigData = data.asInstanceOf[SettingMap[String]];
            val graphics = settings.get[Int]("graphics");
            //body
            glColor3f(0.3f,0.8f,0.3f);
            glPushMatrix;
            {
                glScalef(0.95f,1,1.05f);
                gluQuadrics.sphere.draw(2,graphics*16,graphics*16);
            }
            glPopMatrix
            //ears
            glColor3f(0.4f,0.9f,0.4f);
            glPushMatrix;
            {
                val x = 0.9f;
                glRotatef(180,0,1,0)
                glTranslatef(x,1.7f,-0.7f);
                gluQuadrics.disk.draw(0,0.35f, graphics*8,1);
                glTranslatef(-2*x,0,0);
                gluQuadrics.disk.draw(0,0.35f, graphics*8,1);
            }
            glPopMatrix
            //nose            
            glColor3f(0.4f,1f,0.4f);
            glPushMatrix;
            {
                glScalef(1,1,1);
                //glRotatef(90, 0,1,0)
                glTranslatef(0,0.4f,1.4f);
                val size=0.7f
                gluQuadrics.cylinder.draw(size,size, 1, graphics*12,1);
                glTranslatef(0,0,1);
                gluQuadrics.disk.draw(0,size, graphics*12,1);
            }
            //moustache
            if(pigData.get[Boolean]("Moustache.has")) {
                glScalef(2,1,1);
                glColor3f(0.7f,0.2f,0f);
                pigData.get[Int]("Moustache.which") match {
                    case 0 =>                
                        glTranslatef(0,-0.7f,-0.2f)
                        gluQuadrics.disk.draw(0,0.5f, graphics*9,1);
                    case 1 =>
                        glTranslatef(0,-0.8f,-0.3f)
                        gluQuadrics.partialdisk.draw(0,0.5f, graphics*9,1, 270, 180);
                    case _ =>
                        glTranslatef(0,-0.8f,-0.3f)
                        gluQuadrics.partialdisk.draw(0,0.5f, graphics*9,1, 270, 180);
                }
            }
            glPopMatrix
            //eyes
            glPushMatrix;
            {
                val x = 1.2f;
                def drawEye(leftEye:Boolean) = {
                    glPushMatrix;
                    glColor3f(0.8f,0.8f,0.8f);
                    gluQuadrics.sphere.draw(0.5f,graphics*8,graphics*8);
                    val z = 0.35f;
                    glTranslatef(0,0,z);
                    glColor3f(0.1f,0.1f,0.1f);
                    gluQuadrics.sphere.draw(0.25f,graphics*8,graphics*8);
                    glTranslatef(0,0,0.1f);
                    if(pigData.get[Boolean]("Glasses.has")) {
                        pigData.get[Int]("Glasses.which") match {
                            case 0 => // ray-bans
                                glTranslatef(0,0.2f,0.16f);
                                gluQuadrics.disk.draw(0.60f,0.70f, graphics*10,1);
                                glColor3f(0.20f,0.15f,0.15f);
                                gluQuadrics.disk.draw(0,0.60f, graphics*10,1);
                            case 1 => // monocle
                                if(leftEye){
                                    glColor3f(0.95f,0.8f,0.1f);
                                    gluQuadrics.disk.draw(0.62f,0.70f, graphics*10,1);
                                }
                            case _ => // harry-potter
                                gluQuadrics.disk.draw(0.67f,0.77f, graphics*10,1);
                        }
                    }
                    glPopMatrix
                }
                glTranslatef(x,0.6f,1.2f);
                drawEye(true);
                glTranslatef(-2*x,0,0);
                drawEye(false);
            }
            glPopMatrix
        }
        pig = new GeneratorModel(genPig, drawPig);
        pig.setPosition(0,-worldSize+7f,-worldSize/2+25);
        pig.compile();

        val scale = new Vec3(4f,1f,6.5f)
        catapult = new DisplayModel(()=>{
            glPushMatrix;
            glScalef(scale.x,scale.y,scale.z);
            glColor3f(0.8f,0.3f,0f);
            glBegin(GL_QUADS);                
                // top
                glNormal3f( 0f, 1f, 0f);
                glVertex3f( 1f, 1f,-1f);
                glVertex3f(-1f, 1f,-1f);
                glVertex3f(-1f, 1f, 1f);
                glVertex3f( 1f, 1f, 1f);
                // bottom 
                glNormal3f( 0f,-1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                glVertex3f(-1f,-1f, 1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f( 1f,-1f,-1f);
                // Front
                glNormal3f( 0f, 0f, 1f);
                glVertex3f( 1f, 1f, 1f);
                glVertex3f(-1f, 1f, 1f); 
                glVertex3f(-1f,-1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                // back
                glNormal3f( 0f, 0f,-1f);
                glVertex3f( 1f,-1f,-1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-1f, 1f,-1f);
                glVertex3f( 1f, 1f,-1f);
                // left
                glNormal3f(-1f, 0f, 0f);
                glVertex3f(-1f, 1f, 1f);
                glVertex3f(-1f, 1f,-1f);
                glVertex3f(-1f,-1f,-1f);
                glVertex3f(-1f,-1f, 1f);
                // right
                glNormal3f( 1f, 0f, 0f);
                glVertex3f( 1f, 1f,-1f);
                glVertex3f( 1f, 1f, 1f);
                glVertex3f( 1f,-1f, 1f);
                glVertex3f( 1f,-1f,-1f);
            glEnd;
            glPopMatrix;

            def drawWheel = {
                glRotatef(90, 0,1,0)
                gluQuadrics.cylinder.draw(1f,1f, scale.x*2+2, settings.get[Int]("graphics")*9,1);
                gluQuadrics.disk.draw(0,1,20,1);
                glTranslatef(0,0,scale.x*2+2);
                gluQuadrics.disk.draw(0,1,20,1);
            }
            // Front wheel
            glPushMatrix;
            glTranslatef(-scale.x-1,-1,scale.z-2f)
            drawWheel
            glPopMatrix;
            // Back wheel
            glPushMatrix;
            glTranslatef(-scale.x-1,-1,-scale.z+2f)
            drawWheel
            glPopMatrix;
        });
        catapult.setPosition(0,-worldSize+2.5f,-worldSize/2+25);
        catapult.compile();
        {
        var bbox = new BoundingBox(new Vec3);
        bbox.min -= scale
        bbox.max += scale
        catapult.properties += "box" -> bbox;
        }
        
        pigcatapultLink = new ModelLink(catapult, pig, new Vec3(0f,2.5f,0f));
        //campigLink = new ModelLink(pig, cam, new Vec3(0f,7,-50), new Vec3(0,0,0));
        
        futureTrees += Tree.futureTree;
                
        //generatedModels = models().filter(_.isInstanceOf[GeneratorModel]).map(_.asInstanceOf[GeneratorModel]);
    }
    
    //@ Y is this not in some LWJGL lib, if it's really needed?
    def allocFloats(floatarray:Array[Float]):FloatBuffer = {
        val fb:FloatBuffer = ByteBuffer.allocateDirect(floatarray.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer();
        fb.put(floatarray).asInstanceOf[FloatBuffer].flip();
        return fb;
    }
    
    /**
    * Initial setup of projection of the scene onto screen, lights etc.
    */
    def setupView {
        glClearColor(0.3f,0.6f,0.8f,1f);

        glEnable(GL_DEPTH_TEST); // enable depth buffer (off by default)
        //glEnable(GL_CULL_FACE);  // enable culling of back sides of polygons
        //glCullFace(GL_BACK);
      
        // smooth shading - Gouraud
        glShadeModel(GL_SMOOTH);
        //glShadeModel(GL_FLAT);

        // lights
        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);

        glLight(GL_LIGHT0, GL_AMBIENT, allocFloats(Array[Float](0.3f, 0.3f, 0.3f, 0.0f)));
        glLight(GL_LIGHT0, GL_DIFFUSE, allocFloats(Array[Float](0.7f, 0.7f, 0.7f, 0.0f)));
        glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 20f);
        glLight(GL_LIGHT0, GL_POSITION, allocFloats(Array[Float](0f, 0f, 10f, 0f)));
        glEnable(GL_COLOR_MATERIAL)
        glMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, allocFloats(Array[Float](0.9f, 0.9f, 0.9f, 0f)));
        glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE );
        
        glViewport(0,0, winWidth,winHeight); // mapping from normalized to window coordinates
       
        //glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
        cam.setPerspective(50, winWidth/winHeight.toFloat, 1f, 600f);
        cam.setPosition(0,worldSize-5,-worldSize+5);
        cam.pos = pig.pos;
        cam.setRotation(0,0,0);
    }
  
    /**
    * Resets the view of current frame
    */
    def resetView {
        // clear color and depth buffer
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        //glMatrixMode(GL_PROJECTION);
        //glLoadIdentity;
        //glMatrixMode(GL_MODELVIEW);
        //glLoadIdentity;
    }
  
    var frameIndepRatio = (48000000f);
    var treeView = false;
    var pause = false

    def moveObj = if(treeView) trees(0) else { if(pigcatapultLink.isLinked) catapult else pig };
    
    var fullTimes = 0L
    var renderTimes = 0L
    var physicsTimes = 0L
    var workerTimes = 0L
    
    /**
    * Renders current frame
    */
    def renderFrame = fullTimes += time {
        workerTimes += time {
            // execute max one non-time-critical task per frame
            if(tasks().length>0 && 0.05f+tasks().length/75f > rand.nextFloat) {//if 
            //if((tasks().length>10) || (tasks().length>0 && rand.nextFloat>0.9)) {
                val task = tasks().head;
                task();
                tasks() -= task;
                if(tasks().length==0) println("all tasks done");
            }
        }
    
        if(!pause) {
            physicsTimes += time {
            // pig-catapult collision - hop on catapult :)
            if(!pigcatapultLink.isLinked && !settings.get[Boolean]("air")) {
                var catapultBox = catapult.properties.get[BoundingBox]("box").clone;
                catapultBox.max.x += 2f;
                catapultBox.max.z += 2f;
                catapultBox.min.x -= 2f;
                catapultBox.min.z -= 2f;
                if(catapultBox.pointCollide(pig.pos, catapult.pos)) pigcatapultLink.forgeLink();
            }
            
            // move pig or catapult
            moveObj.vector.z -= 0.05f*moveObj.vector.z*renderTime;
            moveObj.vector.clamp(0,0,8);

            val moveVector = new Vec3(
                math.sin(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z,
                moveObj.vector.y,
                math.cos(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z
            )

            moveObj.pos.clamp(worldSize-2.5f);
            
            val mY = pig.pos.y;
            moveObj.pos += moveVector*renderTime;
            moveObj.pos.clamp(worldSize-2.5f);
            if(pig.pos.y==mY && settings.get[Boolean]("air")) {
                settings += "air"->false;println("pig is on ground");
                val trailcount = 3///
                if(trails.length >= trailcount) 
                    trails = trails.drop(1);
            }
            def unrot(angle:Float, lim:Int=360):Float = { angle - ((math.floor(angle).toInt / lim)*lim) }
            pig.vector += gravity*renderTime
            pig.vector2 -= pig.vector2*renderTime*0.0175f;
            pig.rot += pig.vector2*renderTime
            pig.rot.x = unrot(pig.rot.x);
            var postpigrotx = unrot(pig.rot.x);
            val rotTreshold = 6f;
            
            if(settings.get[Boolean]("air")) {
                trails.last += moveObj.pos;
            } else if(!settings.get[Boolean]("air") && (postpigrotx < rotTreshold || postpigrotx > 360f-rotTreshold)) {
                pig.vector2.x = 0;
                pig.rot.x = 0;
            } else {
                pig.vector2 -= pig.vector2*renderTime*0.01f;//trenje, lol
                if(pig.vector.x < 0.1) pig.vector.x = 0.1;
            }

            // collision detection
            for(tree <- trees) if(tree.visible) {
                var done = false;
                var collision = false;
                val branch = tree.data.asInstanceOf[Branch];
                def dropBranch(b:Branch):GeneratorModel = {
                    b.detach;
                    val rootV = b.rootVec.clone;
                    b.doAll((_.rootVec -= rootV));
                    
                    b.children.foreach((bc)=>{
                        dropBranch(bc);
                    })
                    
                    var drop = new GeneratorModel(()=>{b}, (data:Object)=>data.asInstanceOf[Branch].doAll(_.render));
                    drop.pos = tree.pos + rootV;
                    drop.vector = new Vec3(
                        math.sin(pig.rot.y/(180f/math.Pi)).toFloat*pig.vector.z/2 + rand.nextFloat/2 - rand.nextFloat/2,
                        pig.vector.y/(5 + rand.nextFloat/3 - rand.nextFloat/3), 
                        math.cos(pig.rot.y/(180f/math.Pi)).toFloat*pig.vector.z/2 + rand.nextFloat/2 - rand.nextFloat/2
                    )
                    drop.compile();
                    dropBranches += drop
                    drop;
                }
                
                def getBox(m:DisplayModel):BoundingBox = {
                    var out = m.properties.get[BoundingBox]("box");
                    if(out==null) out = new BoundingBox(new Vec3);
                    out;
                }
                val moveBoxy = getBox(moveObj)
                moveObj.properties += "box" -> moveBoxy;
                def moveBox = moveBoxy offsetBy moveObj.pos

                branch.doWhile((b)=>{!done}, 
                    (b)=>{
                        val box = b.properties.get[BoundingBox]("box");
                        val canCollide = box.pointCollide(pig.pos, tree.pos);
                        if(b.depth==1) {
                            if(canCollide) {
                                val basebox = (new BoundingBox(b.rootVec, b.destVec) offsetBy tree.pos)
                                basebox.min -= 2f;
                                basebox.max.x += 2f;
                                basebox.max.y -= 2f;
                                basebox.max.z += 2f;
                                if(basebox.boxCollide(moveBox)) {
                                    moveObj.vector.z = -moveObj.vector.z;
                                    if(math.abs(moveObj.vector.z) < 0.01f) moveObj.vector.z = 0.01f*math.abs(moveObj.vector.z)/moveObj.vector.z;
                                    var limit = 500;
                                    while(basebox.boxCollide(moveBox) && ({limit -= 1; limit} > 0)) {
                                        val moveVec = new Vec3(
                                            math.sin(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z,
                                            0,
                                            math.cos(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z
                                        )
                                        moveObj.pos += moveVec*renderTime;
                                    }
                                    moveObj.vector.z = moveObj.vector.z/2;
                                    println("bounce");
                                }
                            } else {
                                done = true;
                            }
                        } else if(box.pointCollide(pig.pos, tree.pos)) {
                            collision = true;
                            
                            pig.vector.y /= 2;
                            
                            if(rand.nextFloat > 0.5) {
                                b.children.foreach((bc)=>{if(rand.nextFloat > 0.2) dropBranch(bc)});
                            } else {
                                dropBranch(b);
                            }
                            
                            println("collision");
                            done = true;
                        }
                    }
                );
                if(collision) {
                    tree.compile;
                    tree.reset;

                    var depthSum = 0;
                    val sumLim = 3;
                    branch.doWhile((b)=>{true},(b)=>{ depthSum += 1 });
                    if(depthSum <= sumLim) {// tree is dead
                        println("td:"+depthSum);
                        val drop = dropBranch(branch);
                        drop.vector.y = 2;
                        trees -= tree;
                    }
                }
            }

            // drop branches
            for(branch <- dropBranches) {
                branch.vector += gravity/(5 + rand.nextFloat/3 - rand.nextFloat/3)*renderTime;
                //branch.vector -= branch.vector*renderTime*0.05f;
                branch.pos += branch.vector*renderTime;
                if(branch.pos.y < -worldSize-50) {
                    dropBranches -= branch;
                    if(dropBranches.length==0) println("all broken branches removed");
                }
            }

            pigcatapultLink.applyLink;
            //campigLink.applyLink;
            }
        }
        
        //look at this pig... look!
        cam.lookAt(moveObj)
        val moveVec = new Vec3(
            math.sin(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z,
            0,
            math.cos(moveObj.rot.y/(180f/math.Pi)).toFloat*moveObj.vector.z
        )
        if(moveObj eq pig) cam.angle = new Vec3(0,-1,50); else cam.angle = new Vec3(0,-7,60);
        val camMulti = 10f;
        //cam.pos = ((cam.pos*camMulti) + ((moveObj.pos - new Vec3(0f,0f,-50f))*renderTime))/(camMulti+renderTime);
        cam.pos = ((cam.pos*camMulti) + ((moveObj.pos - cam.angle)*renderTime))/(camMulti+renderTime);
        cam.vector -= cam.vector*renderTime*0.05f;
        cam.render
        
        if(futureTrees.length==0) {
            if(trees.length+futureTrees.length < 5) futureTrees += Tree.futureTree;        
        } else for(futureTree <- futureTrees.clone) if(futureTree.isSet) { 
            val presentTree = futureTree.apply();
            trees += presentTree;
            presentTree.compile();
            futureTrees -= futureTree;
            println("the future is applied: "+System.nanoTime()/1000000L)
        }
    
        // if slow graphics, make trees that are farther away from pig into lines
        /*if(settings.get[Boolean]("fatlines") && (settings.get[Int]("graphics")==1)) for(tree <- trees) {
            if(math.abs((tree.pos-pig.pos).length) > 100 && tree.properties.get[Boolean]("fatlines")) {
                tasks() += (()=>{
                    val ex = settings.get[Boolean]("fatlines");
                    settings += "fatlines" -> false;
                    tree.compile();
                    settings += "fatlines" -> ex;
                })
            } else if(math.abs((tree.pos-pig.pos).length) < 77 && !tree.properties.get[Boolean]("fatlines")) {
                tree.compile();
            }
        }*/
        
        renderTimes += time {
            models().foreach(_.render)
        }
    }
    
    def processInput {
        import Keyboard._ // static imports.. fuckyeah
        
        if(Display.isCloseRequested || isKeyDown(KEY_ESCAPE)) {
            isRunning = false;
            return;
        }
                
        if(isKeyDown(KEY_T) && !timeLock.isLocked) {
            treeView = !treeView
            timeLock.lockIt(500);
        }
        if(isKeyDown(KEY_1)) { settings += "fatlines" -> false; trees.foreach(_.compile()) } else 
        if(isKeyDown(KEY_2)) { settings += "fatlines" -> true; trees.foreach(_.compile()) } else
        if(isKeyDown(KEY_3)) { trees.foreach(_.regenerate()) } else
        if(isKeyDown(KEY_5) && !timeLock.isLocked) { 
            increaseDetail();
            timeLock.lockIt(300);
        } else
        if(isKeyDown(KEY_6) && !timeLock.isLocked && settings.get[Int]("graphics") > 1) { 
            decreaseDetail();
            timeLock.lockIt(300);
        } else
        if(isKeyDown(KEY_8) && !timeLock.isLocked) { pig.regenerate(); timeLock.lockIt(200); } else
        if(isKeyDown(KEY_9) && !timeLock.isLocked) { pig.compile(); timeLock.lockIt(200); }
        
        val keymove = 1.5f*renderTime;
        
        if(isKeyDown(KEY_W)) cam.pos.x+=keymove;
        if(isKeyDown(KEY_R)) cam.pos.x-=keymove;
        if(isKeyDown(KEY_S)) cam.pos.y+=keymove;
        if(isKeyDown(KEY_F)) cam.pos.y-=keymove;
        if(isKeyDown(KEY_X)) cam.pos.z+=keymove;
        if(isKeyDown(KEY_V)) cam.pos.z-=keymove;
/*        if(campigLink.isLinked) {
            if(isKeyDown(KEY_W)) campigLink.vector.x+=keymove;
            if(isKeyDown(KEY_R)) campigLink.vector.x-=keymove;
            if(isKeyDown(KEY_S)) campigLink.vector.y+=keymove;
            if(isKeyDown(KEY_F)) campigLink.vector.y-=keymove;
            if(isKeyDown(KEY_X)) campigLink.vector.z+=keymove;
            if(isKeyDown(KEY_V)) campigLink.vector.z-=keymove;
        } else {
        }*/

        if(isKeyDown(KEY_P)) {
            pause = true;
            println("paused")
        }
        if(isKeyDown(KEY_RSHIFT)) {
            pause = false;
            println("unpaused")
        }
        if(pause) return;

        if(isKeyDown(KEY_Z) && !timeLock.isLocked){
            terrain.visible = !terrain.visible
                
            timeLock.lockIt(100);            
        }

        if(isKeyDown(KEY_LEFT))  moveObj.rot.y+=keymove*3f;
        if(isKeyDown(KEY_RIGHT)) moveObj.rot.y-=keymove*3f;
        if(isKeyDown(KEY_UP))    moveObj.vector.z+=keymove/5f;
        if(isKeyDown(KEY_DOWN))  moveObj.vector.z-=keymove/5f;
        
        if(isKeyDown(KEY_SPACE) && (!settings.get[Boolean]("air"))) {
            if(pigcatapultLink.isLinked) {
                pigcatapultLink.breakLink;
                println("pig-catapult Link Broken")
//                campigLink.breakLink
//                println("cam-pig Link Broken")
                pig.vector.y=3.7f;
                pig.vector.z=7.2f;
                //cam.vector = pig.vector / 3;
                pig.vector2 = new Vec3(0.5f+rand.nextFloat/3,0,0) * 50f;
            } else {
                pig.vector.y=2.7f;
                pig.vector.z=4f;
                if(isKeyDown(KEY_DOWN)) {
                    pig.vector.z = -pig.vector.z;
                    pig.vector2 = -new Vec3(0.5f+rand.nextFloat/3,0,0) * (80f*2.7f/3.7f);
                }
            }
            
            
            settings += "air" -> true;println("pig is in air");
            trails += new TrailModel(List(pig.pos))
        }
        if((isKeyDown(KEY_LCONTROL) || isKeyDown(KEY_RCONTROL))
        && !settings.get[Boolean]("air") && !pigcatapultLink.isLinked) {
            pigcatapultLink.forgeLink;
        }
        
        if(isKeyDown(KEY_O)) {
            println("Cam: "+cam.toString);
            println("Pig: "+pig.toString);
            println("Pig-rot: "+pig.rot.toString);
        }
        if(isKeyDown(KEY_0)) {
            models().foreach(_.reset());
        }
    }
}



