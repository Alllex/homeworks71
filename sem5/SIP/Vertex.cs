using System;

namespace SIP
{
	class Vertex
	{
		static int counter = 0;
		int identifier = counter++;
		int id;
		String lbl;
		int w;

		public Vertex(int id, String lbl, int w)
		{
			this.id = id;
			this.lbl = lbl; 
			this.w = w;
		}

		public int ID { get { return id; } }
		public String Label { get { return lbl; } }
		public int Weight { get { return w; } }
		
		public int CompareTo(object obj)
		{
			if (obj == null) return 1;
			var other = obj as Vertex;
			if (other != null) return this.Equals (other) ? 0 : 1;
			throw new ArgumentException ("Object is not an Vertex");
		}

		public override bool Equals(object obj)
		{
			if (obj == null) return false;
			var other = obj as Vertex;
			if (other != null) return this.identifier == other.identifier;
			throw new ArgumentException ("Object is not an Vertex");
		}

		public override int GetHashCode()
		{
			return identifier ^ (Label.GetHashCode () & id.GetHashCode ()) ^ 37;
		}
	}
}

