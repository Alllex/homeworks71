// SmartCalc.cs
using System;

public class SmartCalc
{

	private static Random rnd = new Random();

	private const int minBound = 1;
	private const int maxBound = 10;

	private static int getRandomNumber() 
	{
		return rnd.Next(minBound, maxBound + 1);
	}

	public static void Main()
    {
    	Console.WriteLine("Я - интеллектуальный калькулятор!");
    	Console.WriteLine("Как тебя зовут?");
    	string userName = Console.ReadLine();
    	while (true) 
    	{
    		int a = getRandomNumber();
    		int b = getRandomNumber();
    		Console.WriteLine("Сколько будет {1} {0} {2}?", "+", a, b);
    		int c;
    		while (!int.TryParse(Console.ReadLine(), out c)) 
    		{
    			Console.WriteLine("Введи число, пожалуйста");
			}
    		if (a + b == c) {
    			Console.WriteLine("Верно, {0}", userName);
    		} else {
    			Console.WriteLine("{0}, ты не прав", userName);
    		}
    	}
    }
}