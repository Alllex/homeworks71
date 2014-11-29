using System;
using QuickGraph;
using System.Collections.Generic;

namespace SIP
{
	using Graph = UndirectedGraph<Vertex, Edge>;
	using WeightedGraph = UndirectedGraph<WeightedVertex, WeightedEdge>;

	class MainClass
	{

		static void print(String s) {
			Console.WriteLine (s);
		}

		static void printWeighted(WeightedGraph q) {
			foreach (var e in q.Edges) {
				print(e.ToString ()); 
			}
		}

		static void test2() {
			var q = new Graph(false);
			Vertex[] qvs = { 
				new Vertex (0, "A"),
				new Vertex (1, "B"),
				new Vertex (2, "B"),
				new Vertex (3, "C")
			};
			Edge[] qes = {
				new Edge (qvs[0], qvs[1]),
				new Edge (qvs[0], qvs[2]),
				new Edge (qvs[0], qvs[3])
			};
			q.AddVertexRange (qvs);
			q.AddEdgeRange (qes);

			var g = new Graph(false);
			Vertex[] gvs = { 
				new Vertex (0, "C"),
				new Vertex (1, "A"),
				new Vertex (2, "A"),
				new Vertex (3, "A"),
				new Vertex (4, "A"),
				new Vertex (5, "B"),
				new Vertex (6, "B"),
				new Vertex (7, "B"),
				new Vertex (8, "B")
			};
			Edge[] ges = {
				new Edge (gvs[0], gvs[1]),
				new Edge (gvs[0], gvs[2]),
				new Edge (gvs[0], gvs[3]),
				new Edge (gvs[0], gvs[4]),

				new Edge (gvs[5], gvs[1]),
				new Edge (gvs[5], gvs[2]),

				new Edge (gvs[6], gvs[1]),
				new Edge (gvs[6], gvs[3]),

				new Edge (gvs[7], gvs[2]),
				new Edge (gvs[7], gvs[4]),

				new Edge (gvs[8], gvs[3]),
				new Edge (gvs[8], gvs[4])
			};
			g.AddVertexRange (gvs);
			g.AddEdgeRange (ges);

			var seq = new SEQ (q, g);
			var lst = seq.GetSEQ ();

			foreach (var item in lst) {
				print (item.ToString ());
			}
		}


		public static void Main (string[] args)
		{
			Console.WriteLine ("Hello World!");
			test2 ();
		}
	}
}

