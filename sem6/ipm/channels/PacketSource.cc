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

#include "PacketSource.h"
#include "DataPacket_m.h"

Define_Module(PacketSource);

void PacketSource::initialize()
{
    Source::initialize();
}

void PacketSource::handleMessage(cMessage *msg)
{
    Source::handleMessage(msg);
}

void PacketSource::finish()
{
    Source::finish();
}

queueing::Job *PacketSource::createJob()
{
    queueing::Job *job = Source::createJob();
    job->addPar("size");
    cMsgPar& p = job->par("size");
    p.setDoubleValue(par("packetSize").doubleValue());
    return job;
}
