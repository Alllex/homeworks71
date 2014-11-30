using System;
using QuickGraph;
using System.Linq;
using System.Collections.Generic;

namespace SIP
{
	using Graph = UndirectedGraph<Vertex, Edge>;
	public class RandomGraph
	{
		RandomGraph (){}

		static Random rnd = new Random();
		static string alphabet = "ABCDEFGHIJKLMNOPRSTUVWXYZ";

		public static Graph GenerateGraph(int vCount, int ePercent)
		{
			Vertex[] vs = new Vertex[vCount];
			for (int i = 0; i < vCount; i++) {
				vs[i] = new Vertex(i, rndLabel ());
			}
			List<Edge> es = new List<Edge> ();
			for (int i = 0; i < vCount; i++) {
				for (int j = i + 1; j < vCount; j++) {
					if (rnd.Next (100) < ePercent) {
						es.Add (new Edge(vs[i], vs[j]));
					}
				}
			}
			var q = new Graph (false);
			q.AddVertexRange (vs);
			q.AddEdgeRange (es);
			return q;
		}

		public static Graph ExtendGraph(Graph q, int vCount, int ePercent)
		{
			var oldVc = q.VertexCount;
			var newVc = oldVc + vCount;

			Vertex[] vs = new Vertex[newVc];
			Vertex[] qvs = q.Vertices.ToArray();

			for (int i = 0; i < newVc; i++) {
				vs [i] = i < oldVc ? qvs [i] : new Vertex (i, rndLabel ());
			}

			bool[,] adjMatrix = new bool[newVc, newVc];
			foreach (var e in q.Edges) {
				var i = Math.Min (e.Source.ID, e.Target.ID);
				var j = Math.Max (e.Source.ID, e.Target.ID);
				adjMatrix [i, j] = true;
			}
			
			List<Edge> es = new List<Edge> ();
			for (int i = 0; i < newVc; i++) {
				for (int j = i + 1; j < newVc; j++) {
					if (rnd.Next (100) < ePercent) {
						adjMatrix [i, j] = true;
					}
					if (adjMatrix [i, j]) {
						es.Add (new Edge(vs[i], vs[j]));
					}
				}
			}

			var g = new Graph (false);
			g.AddVertexRange (vs);
			g.AddEdgeRange (es);
			return g;
		}

		public static Graph MakeConnected(Graph q)
		{
			Vertex[] qvs = q.Vertices.ToArray();
			var g = new Graph (false);
			g.AddVertexRange (q.Vertices);
			g.AddEdgeRange (q.Edges);

			for (int i = 1; i < q.VertexCount; i++) {
				var found = false;
				foreach (var e in q.Edges) {
					if (e.Source.ID == i - 1 && e.Target.ID == i ||
					    e.Target.ID == i - 1 && e.Source.ID == i) {
						found = true;
						break;
					}
				}
				if (!found) {
					g.AddEdge (new Edge(qvs[i - 1], qvs[i]));
				}
			}

			return g;
		}

		public static Graph GenerateConnected(int vCount, int ePercent)
		{
			var q = GenerateGraph (vCount, ePercent);
			return MakeConnected (q);
		}

		static string rndLabel()
		{
			return alphabet[rnd.Next (alphabet.Length)].ToString();
		}
	}
}

