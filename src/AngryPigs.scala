import org.lwjgl._
import org.lwjgl.opengl._
import org.lwjgl.input._
import org.lwjgl.util.glu._
import scala.util._;

object ShowPigs extends Application { 
    (new Pigs()).execute()
};

class Pigs extends Window {
    val rand:Random = new Random();
    
    override def mainLoop() { 
        makeModels();
        
        // setup camera and lights
        setupView();

        while(isRunning) {
            // reset view
            resetView();

            // let subsystem paint
            renderFrame();

            // process input events
            processInput();

            // update window contents and process input messages
            Display.update();
        }
    }
    
    class Point3D(X:Float,Y:Float,Z:Float) {
        def x=X;
        def y=Y;
        def z=Z;
    };
    var terrain:Array[Array[Point3D]]=null;
    
    def makeModels() {
        terrain = Array.ofDim[Point3D](10,10);
        for(i <- 0 until terrain.length; j <- 0 until terrain(i).length)
            terrain(i)(j) = new Point3D(i/10f,rand.nextFloat/2f,j/10f);
    }

    /**
	 * Initial setup of projection of the scene onto screen, lights etc.
	 */
    override def setupView() {
        // enable depth buffer (off by default)
        GL11.glEnable(GL11.GL_DEPTH_TEST); 
        // enable culling of back sides of polygons
        GL11.glEnable(GL11.GL_CULL_FACE);
      
        // mapping from normalized to window coordinates
        GL11.glViewport(0, 0, winX, winY);
	   
        // setup projection matrix stack
        GL11.glMatrixMode(GL11.GL_PROJECTION);
        GL11.glLoadIdentity();
        // orthographic projection 
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
    }
  
    /**
    * Renders current frame
    */
    override def renderFrame() {
        GL11.glBegin(GL11.GL_QUADS);
            for(i <- 0 until terrain.length-1; j <- 0 until terrain(i).length-1) {
                for(ox <- 0 to 1; oy <- 0 to 1) {
                    val p = terrain(i+ox)(j+oy);
                    GL11.glColor3f(p.y, p.y, p.y);
                    GL11.glVertex3f(p.x, p.y, p.z-3);
                }
            }
        GL11.glEnd();
    }
}
