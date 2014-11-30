using System;
using System.Collections.Generic;
using System.Linq;
using QuickGraph;

namespace SIP
{
	using Graph = UndirectedGraph<Vertex, Edge>;
	using WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>;

	public class QuickSI
	{

		Graph q;
		Graph g; 
		List<SEQ.Item> seq; 
		int[] h; 
		bool[] f; 
		int alpha;
		int beta;

		public QuickSI (Graph q, Graph g)
		{
			this.q = q;
			this.g = g;
			this.alpha = g.VertexCount;
			this.beta = q.VertexCount;
			this.h = new int[beta];
			this.f = new bool[alpha];
			this.seq = new SEQ (q, g).GetSEQ ();

			this.Result = algorithm (1);
		}

		public bool Result { get; private set; }

		bool algorithm(int d)
		{
			if (d > beta) {
				return true;
			}
			var t = seq [d - 1];
			//var vs = (from v in g.Vertices
			//	where v.Label == t.Label && !f[v.ID] && (d == 1 || gHasEdge(v.ID, h[t.Parent.ID]))
			//	select v).ToList ();
			var vs = choose (d, t);
			foreach (var v in vs) {
				h [t.Vertex.ID] = v.ID;
				f [v.ID] = true;
				if (algorithm (d + 1)) return true;
				f [v.ID] = false;
			}
			return false;
		}

		List<Vertex> choose(int d, SEQ.Item t)
		{
			var withLabels = (from v in g.Vertices where v.Label == t.Label select v).ToList ();
			var freeVerts = (from v in withLabels where !f [v.ID] select v).ToList ();
			if (d == 1) return freeVerts;
			var withEdges = (from v in freeVerts where gHasEdge (v.ID, h [t.Parent.ID]) select v).ToList ();
			return withEdges;
		}

		bool gHasEdge(int vID, int pID)
		{
			foreach (var e in g.Edges) {
				var src = e.Source.ID;
				var tgt = e.Target.ID;
				if (src == vID && tgt == pID || src == pID && tgt == vID) {
					return true;
				}
			}
			return false;
		}

	}
}

