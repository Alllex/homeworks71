using NUnit.Framework;

namespace Drom
{
    [TestFixture]
    public class Tests
    {

        private static bool IsPalindrom(string s)
        {
            return PalindromDetector.IsPalindrom(s);
        }

        [Test]
        public void TestA()
        {
            Assert.IsTrue(IsPalindrom("A dog! A panic in a pagoda!"));
            Assert.IsTrue(IsPalindrom("A man, a plan, a cat, a ham, a yak, a yam, a hat, a canal-Panama!"));
            Assert.IsTrue(IsPalindrom("Are we not pure? “No sir!” Panama’s moody Noriega brags. “It is garbage!” Irony dooms a man; a prisoner up to new era."));
        }

        [Test]
        public void TestG()
        {
            Assert.IsTrue(IsPalindrom("God’s dog."));
        }

        [Test]
        public void TestO()
        {
            Assert.IsTrue(IsPalindrom("On a clover, if alive, erupts a vast, pure evil; a fire volcano."));
        }

        [Test]
        public void TestP()
        {
            Assert.IsTrue(IsPalindrom("POW, ami! O’ Gad, ami! Go hang a salami, doc! Note; I dissent. A fast never prevents a fatness. I diet on cod. I’m a lasagna hog. I’m a dago! I’m a wop!"));
        }


        [Test]
        public void TestTrueNegative()
        {
            Assert.IsFalse(IsPalindrom("abc"));
            Assert.IsFalse(IsPalindrom("abcb"));
            Assert.IsFalse(IsPalindrom("abc b b"));
            Assert.IsFalse(IsPalindrom("aaaaaaaaaaaaaaacaaaaaaaaaaa"));
            Assert.IsFalse(IsPalindrom("a b                   cxab a"));
        }
    }
}
