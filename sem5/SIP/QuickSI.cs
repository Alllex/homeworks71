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

		Graph g; 
		List<SEQ.Item> seq; 
		int[] h; 
		bool[] f; 
		int alpha;
		int beta;

		public QuickSI (Graph q, Graph g)
		{
			this.g = g;
			this.alpha = g.VertexCount;
			this.beta = q.VertexCount;
			this.h = new int[beta];
			this.f = new bool[alpha];
			this.seq = SEQ.BuildSEQ (q, g);

			this.Result = algorithm (0);
		}

		public bool Result { get; private set; }

		bool algorithm(int d)
		{
			if (d >= beta) {
				return true;
			}
			var t = seq [d];
			var vs = choose (d, t);
			foreach (var v in vs) {
				if (degreeInG (v) < t.Degree ||
				    !t.ExtraEdges.All (p => gHasEdge (v.ID, h [p]))) continue;
				h [t.Vertex] = v.ID;
				f [v.ID] = true;
				if (algorithm (d + 1)) return true;
				f [v.ID] = false;
			}
			return false;
		}

		List<Vertex> choose(int d, SEQ.Item t)
		{
			var freeWithLabels = (from v in g.Vertices where !f [v.ID] && v.Label.Equals (t.Label) select v).ToList ();
			if (d == 0) return freeWithLabels;
			var withEdges = (from v in freeWithLabels where gHasEdge (v.ID, h [t.Parent]) select v).ToList ();
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

		int degreeInG(Vertex v) {
			return g.AdjacentEdges (v).Count ();
		}

	}
}

