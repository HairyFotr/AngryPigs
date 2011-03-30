import org.lwjgl.opengl.GL11;
import org.lwjgl.util.glu.GLU;

object ShowPyramid {
    def main(args: Array[String]) {
        // What version of OpenGL is supported?

        // Start our program
        (new Pyramid()).execute();
    }
}

class Pyramid extends Window {
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
        // perspective projection (45% FOV, 4/3 aspect, clipping near 1, far 30);
        GLU.gluPerspective(45, winX / winY.toFloat, 1.0f, 30.0f);
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
        GL11.glBegin(GL11.GL_TRIANGLES); // draw independent triangles
            GL11.glColor3f(0, 0, 1);
            GL11.glVertex3f(-1.0f, -0.5f, -4.0f);    // lower left vertex
            GL11.glVertex3f( 0.0f,  0.5f, -4.0f);    // upper vertex
            GL11.glVertex3f( 1.0f, -0.5f, -4.0f);    // lower right vertex

            GL11.glColor3f(1, 0, 0);
            GL11.glVertex3f(-1.0f, -0.5f, -4.0f);    // lower left vertex
            GL11.glVertex3f( 1.0f, -0.5f, -4.0f);    // lower right vertex
            GL11.glVertex3f( 0.0f, -0.5f, -3.0f);    // lower front vertex

            GL11.glColor3f(1, 1, 0);
            GL11.glVertex3f(-1.0f, -0.5f, -4.0f);    // lower left vertex
            GL11.glVertex3f( 0.0f, -0.5f, -3.0f);    // lower front vertex
            GL11.glVertex3f( 0.0f,  0.5f, -4.0f);    // upper vertex

            GL11.glColor3f(0, 1, 1);
            GL11.glVertex3f( 1.0f, -0.5f, -4.0f);    // lower right vertex
            GL11.glVertex3f( 0.0f,  0.5f, -4.0f);    // upper vertex
            GL11.glVertex3f( 0.0f, -0.5f, -3.0f);    // lower front vertex
        GL11.glEnd();
    }
}
