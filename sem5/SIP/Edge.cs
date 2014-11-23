using System;
using QuickGraph;

namespace SIP
{
	public class Edge : IEdge<Vertex>, IComparable
	{

		Vertex src;
		Vertex tgt;
		int w;

		public Edge(Vertex src, Vertex tgt, int w)
		{
			this.src = src;
			this.tgt = tgt;
			this.w = w;
		}	

		public Vertex Source { get { return src; } }
		public Vertex Target { get { return tgt; } }
		public int Weight { get { return w; } }

		public int CompareTo(object obj)
		{
			if (obj == null) return 1;
			var other = obj as Edge;
			if (other != null) return this.Source == other.Source && this.Target == other.Target ? 0 : 1;
			throw new ArgumentException ("Object is not an Edge");
		}

		public override bool Equals(object obj)
		{
			if (obj == null) return false;
			var other = obj as Edge;
			if (other != null) return this.Source.Equals(other.Source) && this.Target.Equals(other.Target);
			throw new ArgumentException ("Object is not an Edge");
		}

		public override int GetHashCode()
		{
			return this.Source.GetHashCode () ^ this.Target.GetHashCode ();
		}

	}
}

