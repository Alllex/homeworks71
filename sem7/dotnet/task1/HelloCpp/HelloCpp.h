#pragma once

using namespace HelloProgrammingLang;

ref class HelloCpp : public HelloFSharp
{
public:
	HelloCpp();
	void Meet();
protected:
	void MeetMedved() new;
};

