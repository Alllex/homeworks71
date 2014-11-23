using System;
using System.Collections.Generic;

namespace SIP
{
	public class SEQ
	{

		List<Item> seq;

		SEQ (List<Item> seq)
		{
			this.seq = seq;
		}

		public Item GetItem(int index)
		{
			return seq[index];
		}

		public static class Item
		{
			Vertex p;
			Vertex v;
			String l;

			public Item(Vertex p, Vertex v, String l)
			{
				this.p = p;
				this.v = v;
				this.l = l;
			}

			public Vertex Parent { get { return p; } }
			public Vertex Vertex { get { return v; } }
			public String Label { get { return l; } }
		}

		public static class Builder
		{
			HashSet<Vertex> vs = new HashSet<Vertex>();
			List<Item> seq = new List<Item>();

			public SEQ MakeSEQ()
			{
				return new SEQ (seq);
			}

			public void AddEdge(Edge e)
			{
				Vertex inner = null;
				Vertex outer = null;
				if (vs.Contains (e.Source)) {
					inner = e.Source;
					outer = e.Target;
				} else {
					inner = e.Target;
					outer = e.Source;
				}
				seq.Add(new Item(inner, outer, outer.Label));
			}

		}

	}


}

