#ifndef FASTDDSDATAREADER_LISTENER_IMPL_H
#define FASTDDSDATAREADER_LISTENER_IMPL_H

#include <fastdds/dds/domain/DomainParticipant.hpp>
#include <fastdds/dds/subscriber/DataReaderListener.hpp>

#include <erl_nif.h>

#include <DDS_IDL/FastDDSPSMark.hpp>
#include <DDS_IDL/FastDDSPSMarkPubSubTypes.hpp>

class FastDDSDataReaderListenerImpl
  : public eprosima::fastdds::dds::DataReaderListener {

	ErlNifPid* _listenerPid;
  ErlNifEnv* _env;
  ERL_NIF_TERM _name;
  ERL_NIF_TERM _recv_atom;
  ERL_NIF_TERM _topicName;
  void* _dataContainer;
  eprosima::fastdds::dds::DomainParticipant* _participant;

public:
	FastDDSDataReaderListenerImpl(ErlNifPid* listenerPid, char* clientName, char* topicName, eprosima::fastdds::dds::DomainParticipant* participant) 
    : _listenerPid(listenerPid)
    { 
      _env = enif_alloc_env(); 
      _recv_atom = enif_make_atom(_env, "publish_recv");
      _name = enif_make_string(_env, clientName, ERL_NIF_UTF8); 
      _topicName = enif_make_string(_env, topicName, ERL_NIF_UTF8);
      _participant = participant;
      _dataContainer = PSMark::DeviceMessagePubSubType().create_data();
    }

  void on_data_available(eprosima::fastdds::dds::DataReader* reader) override;
};

#endif /* FASTDDSDATAREADER_LISTENER_IMPL_H */
