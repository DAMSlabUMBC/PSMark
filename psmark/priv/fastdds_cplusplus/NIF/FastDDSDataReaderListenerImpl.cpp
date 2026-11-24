#include <fastdds/dds/subscriber/DataReader.hpp>
#include <NIF/FastDDSDataReaderListenerImpl.h>
#include <memory>

#include <iostream>


void FastDDSDataReaderListenerImpl::on_data_available(eprosima::fastdds::dds::DataReader* reader)
{
    eprosima::fastdds::dds::SampleInfo info;
    while (reader->take_next_sample(_dataContainer, &info) == eprosima::fastdds::dds::RETCODE_OK)
    {
        if (info.valid_data)
        {
            eprosima::fastdds::dds::Time_t recv_time;
            _participant->get_current_time(recv_time);
            unsigned long long fullRecvNs = recv_time.to_ns();

            eprosima::fastdds::rtps::Time_t pub_time = info.source_timestamp;
            unsigned long long fullPubNs = pub_time.to_ns();

            ERL_NIF_TERM recvTimeNs = enif_make_uint64(_env, fullRecvNs);
            ERL_NIF_TERM pubTimeNs = enif_make_uint64(_env, fullPubNs);

            PSMark::DeviceMessage* message = (PSMark::DeviceMessage*)_dataContainer;
            ERL_NIF_TERM payloadSize = enif_make_uint(_env, message->payload().size());
            ERL_NIF_TERM seqId = enif_make_uint64(_env, message->seq_id());
            ERL_NIF_TERM publisherId = enif_make_string(_env, message->publisher_id().c_str(), ERL_NIF_UTF8);
            
            ERL_NIF_TERM data_tuple = enif_make_tuple7(_env, _name, _topicName, seqId, pubTimeNs, recvTimeNs, payloadSize, publisherId);
            ERL_NIF_TERM send_tuple = enif_make_tuple2(_env, _recv_atom, data_tuple);

            enif_send(NULL, _listenerPid, NULL, send_tuple);
        }
    } 
}