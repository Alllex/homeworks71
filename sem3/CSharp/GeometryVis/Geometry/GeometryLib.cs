using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Geometry
{
    public class GeometryLib
    {
        public static bool isCirclesOverlap(int x1, int y1, int r1, int x2, int y2, int r2)
        {
            double d = Math.Sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
            return d < r1 + r2;
        }
    }
}
