import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import scala.util._;

// TODO:
// search for @task

object AngryPigs {

    var isRunning = false; // is main loop running
    val (winWidth, winHeight)=(800,600); // window size
    val cam = new Camera;
    val rand = new Random;
    
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
        mainLoop;
        
        // cleanup
        Display.destroy;
    }

    def initDisplay {
        var bestMode:DisplayMode = null;
        val modes:Array[DisplayMode] = Display.getAvailableDisplayModes;
        // Get best mode
        for(mode <- modes)
            if((mode.getWidth == winWidth && mode.getHeight == winHeight && mode.getFrequency <= 85)
                &&(bestMode == null
                   ||(mode.getBitsPerPixel >= bestMode.getBitsPerPixel
                      && mode.getFrequency > bestMode.getFrequency)))
                bestMode = mode;
        
        Display.setDisplayMode(bestMode);
        // FSAA
        //Display.create(new PixelFormat(8, 8, 8, 4));
        // No FSAA
        Display.create;
        Display.setTitle("Angry Pigs");
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
            frameTime = now;
            
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
    var pigcatapultLink:ModelLink=null;
    //size of world
    val worldSize = 100;
    val gravity = new Vec3(0f,-0.5f,0f);
    
    // @would it pay-off to make model generation lazy and generate them on the fly?
    // @infinite terrain patches and stuff
    def makeModels {
        // terrain
        val detail=10;
        val height=0.2f;
        
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
                // top
                GL11.glColor3f(0f,1f,0f); // green
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                // bottom 
                /*GL11.glColor3f(1f,0.5f,0f);    // orange
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f( 1f,-1f,-1f);*/
                // Front
                GL11.glColor3f(1f,0f,0f); // red 
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f, 1f); 
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                // back
                GL11.glColor3f(1f,1f,0f); // yellow
                GL11.glVertex3f( 1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f,-1f);
                // left
                GL11.glColor3f(0f,0f,1f); // blue
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                // right
                GL11.glColor3f(1f,0f,1f); // violet
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f,-1f);
            GL11.glEnd;
        });
        skybox.setPosition(0,0,0);
        skybox.setScale(worldSize,worldSize,worldSize);//*/

        // pig
        pig = new DisplayModel(Unit=>{
            GL11.glColor3f(0.2f,0.7f,0.2f);
            val p = new Sphere();
            GL11.glScalef(0.95f,1,1.1f);
            p.draw(2,15,15);
            GL11.glScalef(1,1,1);
            GL11.glTranslatef(0,1.4f,1.9f);
            p.draw(0.9f,10,10);
        });
        pig.setPosition(0,-worldSize+2.5f,-worldSize+25);
        //pig.setScale(worldSize/,worldSize,worldSize);//*/

        catapult = new DisplayModel(Unit=>{
            GL11.glBegin(GL11.GL_QUADS);
                GL11.glColor3f(1f,0.5f,0f); // orange

                // top
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                // bottom 
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f( 1f,-1f,-1f);
                // Front
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f, 1f); 
                GL11.glVertex3f(-1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                // back
                GL11.glVertex3f( 1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f,-1f);
                // left
                GL11.glVertex3f(-1f, 1f, 1f);
                GL11.glVertex3f(-1f, 1f,-1f);
                GL11.glVertex3f(-1f,-1f,-1f);
                GL11.glVertex3f(-1f,-1f, 1f);
                // right
                GL11.glVertex3f( 1f, 1f,-1f);
                GL11.glVertex3f( 1f, 1f, 1f);
                GL11.glVertex3f( 1f,-1f, 1f);
                GL11.glVertex3f( 1f,-1f,-1f);
            GL11.glEnd;
        });
        catapult.setPosition(0,-worldSize+2.5f,-worldSize+25);
        catapult.setScale(3,1,3);//*/
        
        pigcatapultLink = new ModelLink(catapult, pig, new Vec3(0f,1f,0.5f));
    }

    /**
     * Initial setup of projection of the scene onto screen, lights etc.
     */
    def setupView {
        GL11.glEnable(GL11.GL_DEPTH_TEST); // enable depth buffer (off by default)
        //GL11.glEnable(GL11.GL_CULL_FACE);  // enable culling of back sides of polygons
        //GL11.glCullFace(GL11.GL_BACK);
      
        GL11.glViewport(0,0, winWidth,winHeight); // mapping from normalized to window coordinates
       
        GL11.glHint(GL11.GL_PERSPECTIVE_CORRECTION_HINT, GL11.GL_NICEST);
        cam.setPerspective(50, winWidth/winHeight.toFloat, 1f, 500f);
        cam.setPosition(0,worldSize-5,-worldSize+5);
        cam.setRotation(0,180,0);
    }
  
    /**
    * Resets the view of current frame
    */
    def resetView {
        // clear color and depth buffer
        GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity;
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity;
    }
  
    //avoid spikes in Rendertime (used for input)
    var normalizedRenderTime = -1f;
    var frameIndepRatio = (10000000f/2f);
    
    /**
    * Renders current frame
    */
    def renderFrame {
        var renderTime = (now-frameTime)/frameIndepRatio;
        if(normalizedRenderTime == -1) {
            normalizedRenderTime = renderTime;
        } else {
            normalizedRenderTime = (renderTime+normalizedRenderTime*2)/3;
        }
        
        val toRender = List(
            cam,
            terrain,
            skybox,
            coordsys,
            pig,
            catapult
        )
        cam.pos.clamp(worldSize-5);

        pig.pos.applyVector(pig.vector*normalizedRenderTime);
        pig.vector.applyVector(gravity*normalizedRenderTime);
        
        pig.pos.clamp(worldSize-2.5f);
        pigcatapultLink.applyLink;
        //val pigpos = pig.pos.clone;
        //if(pigpos != pig.pos) pig.vector=new Vec3(0,0,0);
        
        toRender.map(_.render);
    }
    
    def processInput {
        if(Display.isCloseRequested || Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) isRunning = false;
        
        val keymove = 0.7f*normalizedRenderTime;
        
        if(Keyboard.isKeyDown(Keyboard.KEY_Q)) cam.rot.x+=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_E)) cam.rot.x-=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_A)) cam.rot.y+=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_D)) cam.rot.y-=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_Y)) cam.rot.z+=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_C)) cam.rot.z-=keymove;

        if(Keyboard.isKeyDown(Keyboard.KEY_W)) cam.pos.x+=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_R)) cam.pos.x-=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_S)) cam.pos.y+=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_F)) cam.pos.y-=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_X)) cam.pos.z+=keymove;
        if(Keyboard.isKeyDown(Keyboard.KEY_V)) cam.pos.z-=keymove;

        if(Keyboard.isKeyDown(Keyboard.KEY_LEFT))  pig.vector.x+=keymove/3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_RIGHT)) pig.vector.x-=keymove/3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_UP))    pig.vector.z+=keymove/3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_DOWN))  pig.vector.z-=keymove/3f;
        if(Keyboard.isKeyDown(Keyboard.KEY_RCONTROL)) {
            pig.vector.x *= 0.8f;
            pig.vector.z *= 0.8f;
        }
        // x+z vectors should be clamped
        if(pigcatapultLink.isLinked){
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
        }

        if(Keyboard.isKeyDown(Keyboard.KEY_SPACE) && pigcatapultLink.isLinked) {
            pig.vector.y=5.5f;
            pig.vector.z=7f;
            pigcatapultLink.breakLink;
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_LCONTROL)) {
            pigcatapultLink.forgeLink;
        }

        if(Keyboard.isKeyDown(Keyboard.KEY_P)) {
            println(cam.toString);
            println(pig.toString);
        }
        if(Keyboard.isKeyDown(Keyboard.KEY_0)) {
            cam.setPosition(0,worldSize-5,-worldSize+5);
            cam.setRotation(0,180,0);
        }
    }    
}






