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
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private System.Drawing.Graphics g;
        private System.Drawing.Pen penFill = new System.Drawing.Pen(Color.White, 500F);
        private System.Drawing.Pen pen1 = new System.Drawing.Pen(Color.Blue, 2F);
        private System.Drawing.Pen pen2 = new System.Drawing.Pen(Color.Red, 2F);

        private void button1_Click(object sender, EventArgs e)
        {
            int h = pictureBox1.Height;
            int w = pictureBox1.Width;
            g = pictureBox1.CreateGraphics();
            g.DrawRectangle(penFill, 0, 0, w, h);
            Random rnd = new Random();
            int x1 = rnd.Next(0, w / 2);
            int y1 = rnd.Next(0, h / 2);
            int x2 = rnd.Next(0, w / 2);
            int y2 = rnd.Next(0, h / 2);
            int r1 = rnd.Next(Math.Min(h, w) / 20, Math.Min(h, w) / 3);
            int r2 = rnd.Next(Math.Min(h, w) / 20, Math.Min(h, w) / 3);
            g.DrawEllipse(pen1, x1, y1, r1 * 2, r1 * 2);
            g.DrawEllipse(pen2, x2, y2, r2 * 2, r2 * 2);

            if (GeometryLib.isCirclesOverlap(x1 + r1, y1 + r1, r1, x2 + r2, y2 + r2, r2))
            {
                label1.Text = "Overlaps!!!";
            }
            else
            {
                label1.Text = "They are too far from each other...";
            }
        }

    }
}
