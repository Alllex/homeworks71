//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public License
// along with this program.  If not, see http://www.gnu.org/licenses/.
// 

#include "DataDelay.h"

Define_Module(DataDelay);

void DataDelay::initialize()
{
    Delay::initialize();
    errorRate = par("errorRate").doubleValue();
    speed = par("speed").doubleValue();
}

void DataDelay::handleMessage(cMessage *msg)
{
    double size = msg->par("size").doubleValue();
    double delay = size / speed;
    par("delay").setDoubleValue(delay);
    if (!msg->isSelfMessage()) {
        Delay::handleMessage(msg);
        return;
    }
    if (uniform(0.0, 1.0) < errorRate) {
        EV << "Packet is to be resent because of an error: " << msg << endl;
        scheduleAt(simTime() + delay, msg);
        return;
    }
    Delay::handleMessage(msg);
}
