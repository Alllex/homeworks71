using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Geometry;

namespace GeometryVis
{
    public partial class MainForm : Form
    {

        Circle c1 = null;
        Circle c2 = null;

        public MainForm()
        {
            InitializeComponent();

            int w = pictureBox1.Width;
            int h = pictureBox1.Height;

            c1 = new Circle(new Pen(Color.Red, 3F), w, h);
            c2 = new Circle(new Pen(Color.Blue, 3F), w, h);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            c1.relocateAndResize();
            c2.relocateAndResize();
            pictureBox1.Refresh();
            updateLabel();
        }

        private void updateLabel()
        {
            if (Circle.areOverlap(c1, c2))
            {
                label1.Text = "They are overlap!";
            }
            else
            {
                label1.Text = "They are too far from each other...";
            }
        }

        private void pictureBox1_Paint(object sender, PaintEventArgs e)
        {
            e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;
            e.Graphics.FillRectangle(Brushes.White, 0, 0, pictureBox1.Width, pictureBox1.Height);
            c1.draw(e.Graphics);
            c2.draw(e.Graphics);
        }

    }

    class Circle
    {

        static Random rnd = new Random();

        int top = 0;
        int left = 0;
        int r = 0;
        Pen pen = null;

        int maxW = 0;
        int maxH = 0;
        int maxR = 0;

        public Circle(Pen p, int boxW, int boxH)
        {
            pen = p;
            maxW = boxW;
            maxH = boxH;
            maxR = Math.Min(boxW, boxH);
            relocateAndResize();
        }

        public void relocateAndResize()
        {
            left = rnd.Next(0, maxW / 2);
            top = rnd.Next(0, maxH / 2);
            r = rnd.Next(maxR / 20, maxR / 3);
        }

        static public bool areOverlap(Circle c1, Circle c2)
        {
            return GeometryLib.isCirclesOverlap(c1.centerX(), c1.centerY(), c1.radius(), 
                                                c2.centerX(), c2.centerY(), c2.radius()); 
        }

        public void draw(Graphics g)
        {
            g.DrawEllipse(pen, left, top, diameter(), diameter());
        }

        public int centerX()
        {
            return left + r;
        }

        public int centerY()
        {
            return top + r;
        }

        public int diameter()
        {
            return r * 2;
        }

        public int radius()
        {
            return r;
        }
    }
}
