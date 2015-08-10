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

#include "SinkStat.h"
#include "Job.h"
#include "DataPacket_m.h"

Define_Module(SinkStat);

typedef queueing::Job Job;

void SinkStat::initialize()
{
    Sink::initialize();
    totalSize = 0.0;
}

void SinkStat::handleMessage(cMessage *msg)
{
    double size = msg->par("size").doubleValue();
    totalSize += (long long)size;
    EV << "Received packet size: " << size << endl;
    Sink::handleMessage(msg);
}

void SinkStat::finish()
{
    long long t3 = totalSize % 1000;
    long long t6 = (totalSize % (1000 * 1000)) / 1000;
    long long t9 = (totalSize % (1000 * 1000 * 1000)) / (1000 * 1000);
    long long t12 = totalSize / (1000 * 1000 * 1000); //(totalSize % (1000 * 1000 * 1000 * 1000)) / (1000 * 1000 * 1000);
    EV << "Data channels amount: " << par("dataChAmount").longValue() << endl;
    EV << "Control channels amount: " << par("ctrlChAmount").longValue() << endl;
    EV << "Total size of received packets: ";
    if (t12 > 0) EV << t12 << " ";
    EV << t9 << " " << t6 << " " << t3 << " (" << totalSize << ")" << endl;
    Sink::finish();
}
