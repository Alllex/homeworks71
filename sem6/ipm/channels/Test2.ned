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

package test2;

import org.omnetpp.queueing.Source;
import org.omnetpp.queueing.Queue;
import org.omnetpp.queueing.Sink;
import org.omnetpp.queueing.Delay;
import org.omnetpp.queueing.ResourcePool;
import org.omnetpp.queueing.Allocate;
import org.omnetpp.queueing.Deallocate;


network Test2
{
    submodules:
        source: PacketSource {
            @display("p=50,113");
        }
        sink: SinkStat {
            @display("p=425,160");
        }
        dataChPool: ResourcePool {
            @display("p=50,35");
        }
        ctrlChPool: ResourcePool {
            @display("p=165,35");
        }
        dataChAlloc: Allocate {
            @display("p=140,113");
        }
        ctrlChAlloc: Allocate {
            @display("p=140,206");
        }
        dataChDealloc: Deallocate {
            @display("p=50,206");
        }
        ctrlChDealloc: Deallocate {
            @display("p=327,160");
        }
        dataChDelay: DataDelay {
            @display("p=233,113");
        }
        ctrlChDelay: Delay {
            @display("p=233,206");
        }
    connections:
        source.out --> dataChAlloc.in++;
        dataChAlloc.out --> dataChDelay.in++;
        dataChDelay.out --> dataChDealloc.in++;
        dataChDealloc.out --> ctrlChAlloc.in++;
        ctrlChAlloc.out --> ctrlChDelay.in++;
        ctrlChDelay.out --> ctrlChDealloc.in++;
        ctrlChDealloc.out --> sink.in++;
}
