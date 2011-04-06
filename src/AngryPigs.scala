import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import scala.util._;

// TODO:
// search for @task
// school examples most likely suck

object ShowPigs extends Application { (new Pigs()).execute() }

class Pigs extends Window {
    val rand:Random = new Random();
    
    override def mainLoop() { 
        //loadModels(); // load models
        makeModels(); // make generative models
        setupView();  // setup camera and lights

        while(isRunning) {            
            resetView();      // reset view
            renderFrame();    // let subsystem paint
            processInput();   // process input events @menda se da sproti gledat
            Display.update(); // update window contents and process input messages
        }
    }
    
    var terrain:TexQuadPatch=null;
    // @would it pay-off to make model generation lazy and generate them on the fly?
    def makeModels() {
        // Terrain
        val detail=50;
        val height=0.3f;
        
        def getTerrainPoint(x:Int, y:Int):Vec3 = new Vec3(x/detail.toFloat,rand.nextFloat*height,y/detail.toFloat);
        val p = (for(i <- 0 until detail; j <- 0 until detail) yield getTerrainPoint(i,j)).toArray;
        
        terrain = new TexQuadPatch(p, detail);
    }

    /**
	 * Initial setup of projection of the scene onto screen, lights etc.
	 */
    override def setupView() {
        GL11.glEnable(GL11.GL_DEPTH_TEST); // enable depth buffer (off by default)
        GL11.glEnable(GL11.GL_CULL_FACE); // enable culling of back sides of polygons
      
        GL11.glViewport(0, 0, winX, winY); // mapping from normalized to window coordinates
	   
        GL11.glMatrixMode(GL11.GL_PROJECTION); // setup projection matrix stack
        GL11.glLoadIdentity();
        
        // orthographic projection (...) 
        //GL11.glOrtho(-5,5,-5,5,1,30);
        // perspective projection (FOV, aspect, clipping near, clipping far);
        GLU.gluPerspective(45, winX/winY.toFloat, 1.0f, 100.0f);
    }
  

    /**
    * Resets the view of current frame
    */
    override def resetView() {
        // clear color and depth buffer
        GL11.glClear(GL11.GL_COLOR_BUFFER_BIT | GL11.GL_DEPTH_BUFFER_BIT);
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity();
        GLU.gluPerspective(45, winX/winY.toFloat, 1.0f, 100.0f);
        GL11.glRotatef(10f,rot.x,rot.y,rot.z);
        GL11.glTranslatef(tran.x,tran.y,tran.z);
        GL11.glMatrixMode(GL11.GL_MODELVIEW);
        GL11.glLoadIdentity();
    }
  
    // camera rotation and translation
    // @replace with camera object, ofcourse
    var (rot,tran)=(new Vec3(17.5f,-10f,-3.3f),new Vec3(-0.9f,-1.5f,-0.5f));//(new Vec3,new Vec3);
 
    /**
    * Renders current frame
    */
    override def renderFrame() {
        terrain.render();
    }
    
    override def processInput() {
        if(Display.isCloseRequested() || Keyboard.isKeyDown(Keyboard.KEY_ESCAPE)) isRunning = false;
        
        if(Keyboard.isKeyDown(Keyboard.KEY_Q)) rot.x+=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_E)) rot.x-=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_A)) rot.y+=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_D)) rot.y-=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_Y)) rot.z+=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_C)) rot.z-=0.1f;

        if(Keyboard.isKeyDown(Keyboard.KEY_W)) tran.x+=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_R)) tran.x-=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_S)) tran.y+=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_F)) tran.y-=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_X)) tran.z+=0.1f;
        if(Keyboard.isKeyDown(Keyboard.KEY_V)) tran.z-=0.1f;
        
        if(Keyboard.isKeyDown(Keyboard.KEY_P)) {
            println(tran.x+" "+tran.y+" "+tran.z);
            println(rot.x+" "+rot.y+" "+rot.z);
        }
    }
    
}






