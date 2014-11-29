using System;

namespace SIP
{
	public class WeightedVertex : Vertex
	{
		
		public WeightedVertex(int id, String lbl, int w): base(id, lbl)
		{
			this.Weight = w;
		}

		public int Weight { get; private set; }

		public override string ToString ()
		{
			return string.Format ("[WV: ID={0}, L={1}, W={2}]", ID, Label, Weight);
		}
	}
}

