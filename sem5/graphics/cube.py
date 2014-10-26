from OpenGL.GL import *
from OpenGL.GLUT import *
from OpenGL.GLU import * 

W = 500
SIDE = W / 2
DISTANCE = 0
(LEFT, RIGHT) = (-SIDE / 2, SIDE / 2)
(BOTTOM, TOP) = (-SIDE / 2, SIDE / 2)
(FAR, NEAR)   = (-SIDE / 2, SIDE / 2)
FAR  -= DISTANCE + SIDE / 2
NEAR -= DISTANCE + SIDE / 2

VIEW_ORTHO  = 0
VIEW_FOCUS1 = 1
VIEW_FOCUS2 = 2
VIEW_FOCUS3 = 3

VIEWS_COUNT = 4
VIEW_NUMBER = 0

def key_pressed(key, x, y):
    global VIEW_NUMBER
    if key == '\033': sys.exit()
    elif key == ' ': VIEW_NUMBER = (VIEW_NUMBER + 1) % VIEWS_COUNT
 
def draw_sides():

    glBegin(GL_QUADS)

    glColor3f(1, 0, 0) # LEFT side
    glVertex3f(LEFT, BOTTOM, NEAR)
    glVertex3f(LEFT, BOTTOM, FAR)
    glVertex3f(LEFT, TOP, FAR)
    glVertex3f(LEFT, TOP, NEAR)

    glColor3f(0, 1, 0) # RIGHT side
    glVertex3f(RIGHT, BOTTOM, NEAR)
    glVertex3f(RIGHT, BOTTOM, FAR)
    glVertex3f(RIGHT, TOP, FAR)
    glVertex3f(RIGHT, TOP, NEAR)

    glColor3f(0, 0, 0.5) # BOTTOM side
    glVertex3f(RIGHT, BOTTOM, NEAR)
    glVertex3f(RIGHT, BOTTOM, FAR)
    glVertex3f(LEFT, BOTTOM, FAR)
    glVertex3f(LEFT, BOTTOM, NEAR)

    glColor3f(0.9, 0.9, 0.6) # TOP side
    glVertex3f(RIGHT, TOP, NEAR)
    glVertex3f(RIGHT, TOP, FAR)
    glVertex3f(LEFT, TOP, FAR)
    glVertex3f(LEFT, TOP, NEAR)

    glColor3f(0.7, 0.7, 0.3) # FAR side
    glVertex3f(RIGHT, BOTTOM, FAR)
    glVertex3f(RIGHT, TOP, FAR)
    glVertex3f(LEFT, TOP, FAR)
    glVertex3f(LEFT, BOTTOM, FAR)

    glEnd() 

def load_ortho():
    glOrtho(2 * LEFT, 2 * RIGHT, 2 * BOTTOM, 2 * TOP, -1, 2 * (DISTANCE + SIDE))

def load_persp(view_angle=60):
    gluPerspective(view_angle, 1, 0, DISTANCE + 2 * SIDE)

def draw_scene():

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glViewport(0, 0, W, W)
    glLoadIdentity()
    glMatrixMode(GL_PROJECTION)

    if   VIEW_NUMBER == VIEW_ORTHO:  load_ortho()
    elif VIEW_NUMBER == VIEW_FOCUS1: load_persp(45)
    elif VIEW_NUMBER == VIEW_FOCUS2: load_persp(60)
    elif VIEW_NUMBER == VIEW_FOCUS3: load_persp(75)
    else: 
        print 'Invalid view', VIEW_NUMBER
        sys.exit()

    draw_sides()
    glutSwapBuffers()

def main():
    glutInit()
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH)

    glutInitWindowSize(W, W)
    glutInitWindowPosition(300, 100)
    glutCreateWindow('Task #1 -- Cube (Press Space for next view)')

    glutDisplayFunc(draw_scene)
    glutIdleFunc(draw_scene)
    glutKeyboardFunc(key_pressed)

    glutMainLoop()
 

if __name__ == "__main__":
        main() 
