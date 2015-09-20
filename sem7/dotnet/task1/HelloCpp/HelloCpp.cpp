#include "stdafx.h"
#include "HelloCpp.h"
#include <iostream>

HelloCpp::HelloCpp()
{
}

void HelloCpp::Meet()
{
	MeetMedved();
}

void HelloCpp::MeetMedved()
{
	std::cout << "Preved from C++" << std::endl;
	__super::MeetMedved();
}
