using System;

namespace Drom
{

    class PalindromDetector
    {

        private static bool IsDelimiter(char c)
        {
            return !Char.IsLetterOrDigit(c);
        }

        private static bool AreEqual(char a, char b)
        {
            return Char.ToLower(a).Equals(Char.ToLower(b));
        }

        public static bool IsPalindrom(string s)
        {
            int i = 0;
            int j = s.Length - 1;
            do
            {
                while (IsDelimiter(s[i])) i++;
                while (IsDelimiter(s[j])) j--;
                if (!AreEqual(s[i++], s[j--]))
                {
                    return false;
                }
            } while (i < j - 1);
            return true;
        }

    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Type a string:");
            string s = Console.ReadLine();
            if (PalindromDetector.IsPalindrom(s))
            {
                Console.WriteLine("It is palindrom.");
            } else {
                Console.WriteLine("This string won't survive");
            }
        }
    }
}
