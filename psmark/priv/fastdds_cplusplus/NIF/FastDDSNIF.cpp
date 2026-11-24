#include <fastdds/dds/domain/DomainParticipant.hpp>
#include <fastdds/dds/domain/DomainParticipantFactory.hpp>
#include <fastdds/dds/publisher/Publisher.hpp>
#include <fastdds/dds/publisher/DataWriter.hpp>
#include <fastdds/dds/subscriber/Subscriber.hpp>

#include <erl_nif.h>

#include <DDS_IDL/FastDDSPSMark.hpp>
#include <DDS_IDL/FastDDSPSMarkPubSubTypes.hpp>
#include <NIF/FastDDSDataReaderListenerImpl.h>

#include <vector>
#include <string>

// Define macro to avoid compiler warnings for unused API params
#define UNUSED_PARAM(expr) do { (void)(expr); } while (0)
#define MAX_SAMPLES 1000

ErlNifResourceType* PARTICIPANT_RES_TYPE;
ErlNifResourceType* PUBLISHER_RES_TYPE;
ErlNifResourceType* SUBSCRIBER_RES_TYPE;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_not_connected;
ERL_NIF_TERM atom_recv;

typedef struct 
{
    eprosima::fastdds::dds::DomainParticipant* participant;
} ParticipantStruct;

typedef struct 
{
    eprosima::fastdds::dds::Publisher* publisher;
    eprosima::fastdds::dds::DataWriter* writer;
    char* nodeId;
    void* dataContainer;
} PublisherStruct;

typedef struct 
{
    eprosima::fastdds::dds::Subscriber* subscriber;
} SubscriberStruct;

typedef struct 
{
    ErlNifPid* pid;
    ErlNifEnv* env;
    ERL_NIF_TERM name;
    ERL_NIF_TERM topicName;
} ReaderListenerData;

void participant_free_res(ErlNifEnv* env, void* obj)
{
    UNUSED_PARAM(env);

    ParticipantStruct* participantStruct = (ParticipantStruct*) obj;
    participantStruct->participant->delete_contained_entities();
    eprosima::fastdds::dds::DomainParticipantFactory::get_instance()->delete_participant(participantStruct->participant);
}

void publisher_free_res(ErlNifEnv* env, void* obj)
{
    UNUSED_PARAM(env);
    
    PublisherStruct* publisherStruct = (PublisherStruct*) obj;

    if(publisherStruct->nodeId != 0)
    {
        free(publisherStruct->nodeId);
        publisherStruct->nodeId = 0;
    }

    PSMark::DeviceMessagePubSubType().delete_data(publisherStruct->dataContainer);
}

void subscriber_free_res(ErlNifEnv* env, void* obj)
{
    UNUSED_PARAM(env);
    UNUSED_PARAM(obj);
    // Do nothing, cleaned with participant
}

static int open_resource(ErlNifEnv* env)
{
    const char* mod = NULL;
    const char* name = "ParticipantStruct";
    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

    PARTICIPANT_RES_TYPE = enif_open_resource_type(env, mod, name, participant_free_res, flags, NULL);
    if(PARTICIPANT_RES_TYPE == NULL) return -1;

    name = "PublisherStruct";
    PUBLISHER_RES_TYPE = enif_open_resource_type(env, mod, name, publisher_free_res, flags, NULL);
    if(PUBLISHER_RES_TYPE == NULL) return -1;

    name = "SubscriberStruct";
    SUBSCRIBER_RES_TYPE = enif_open_resource_type(env, mod, name, subscriber_free_res, flags, NULL);
    if(SUBSCRIBER_RES_TYPE == NULL) return -1;

    return 0;
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    UNUSED_PARAM(priv);
    UNUSED_PARAM(load_info);

    if(open_resource(env) == -1) return -1;

    atom_ok = enif_make_atom(env, "ok");
    atom_not_connected = enif_make_atom(env, "not_connected");
    atom_recv = enif_make_atom(env, "publish_recv");

    return 0;
}

// Erlang requires that we re-open resources on re-initialisation.
static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    UNUSED_PARAM(priv);
    UNUSED_PARAM(load_info);

    if(open_resource(env) == -1) return -1;
    return 0;
}

static int reload(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    UNUSED_PARAM(priv);
    UNUSED_PARAM(old_priv);
    UNUSED_PARAM(load_info);

    if(open_resource(env) == -1) return -1;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
    UNUSED_PARAM(env);
    UNUSED_PARAM(priv);
    UNUSED_PARAM(old_priv);
    UNUSED_PARAM(load_info);

    return 0;
}

static void unload(ErlNifEnv* caller_env, void* priv_data)
{
    UNUSED_PARAM(caller_env);
    UNUSED_PARAM(priv_data);
}

ERL_NIF_TERM create_participant(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    ParticipantStruct* res;
    ERL_NIF_TERM ret;

    int domainId = 0;
    if(!enif_get_int(env, argv[0], &domainId))
    {
        return enif_make_badarg(env);
    }

    // Get config file path
    unsigned int configPathLength = 0;
    if(!enif_get_string_length(env, argv[1], &configPathLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_config_path");
    }

    char* configPath = (char*)malloc(configPathLength + 1);
    if(!enif_get_string(env, argv[1], configPath, configPathLength + 1, ERL_NIF_UTF8))
    {
        free(configPath);
        return enif_make_atom(env, "invalid_config_path");
    }

    // Get QoS profile
    unsigned int qosParamLength = 0;
    if(!enif_get_string_length(env, argv[2], &qosParamLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_profile");
    }

    bool useQos = false;
    char* qosParam = nullptr;
    if(qosParamLength > 0)
    {
        useQos = true;
        qosParam = (char*)malloc(qosParamLength + 1);
        if(!enif_get_string(env, argv[2], qosParam, qosParamLength + 1, ERL_NIF_UTF8))
        {
            free(qosParam);
            return enif_make_atom(env, "invalid_qos_profile");
        }
    }

    // Get XML QoS
    eprosima::fastdds::dds::ReturnCode_t retCode = eprosima::fastdds::dds::DomainParticipantFactory::get_instance()->load_XML_profiles_file(configPath);
    if(retCode != eprosima::fastdds::dds::RETCODE_OK)
    {
        return enif_make_atom(env, "malformed_qos");
    }

    // Get domain participant QoS
    eprosima::fastdds::dds::DomainParticipantQos participantQoS;
    retCode = eprosima::fastdds::dds::DomainParticipantFactory::get_instance()->get_participant_qos_from_profile(qosParam, participantQoS);
    if(retCode != eprosima::fastdds::dds::RETCODE_OK)
    {
        return enif_make_atom(env, "participant_profile_not_found");
    }

    eprosima::fastdds::dds::DomainParticipant* participant = eprosima::fastdds::dds::DomainParticipantFactory::get_instance()->create_participant(domainId, participantQoS);
    if(participant == nullptr)
    {
        return enif_make_atom(env, "participant_failed");
    }

    // Register type
    eprosima::fastdds::dds::TypeSupport* typeSupport = new eprosima::fastdds::dds::TypeSupport(new PSMark::DeviceMessagePubSubType());
    typeSupport->register_type(participant, "DeviceMessage");

    res = (ParticipantStruct*) enif_alloc_resource(PARTICIPANT_RES_TYPE, sizeof(ParticipantStruct));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->participant = participant;

    return enif_make_tuple2(env, atom_ok, ret);
}

ERL_NIF_TERM create_subscriber_on_topic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    // Get topic
    unsigned int topicLength = 0;
    if(!enif_get_string_length(env, argv[0], &topicLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_topic");
    }

    char* topicName = (char*)malloc(topicLength + 1);
    if(!enif_get_string(env, argv[0], topicName, topicLength + 1, ERL_NIF_UTF8))
    {
        free(topicName);
        return enif_make_atom(env, "invalid_topic");
    }

    // Get client name
    unsigned int clientNameLength = 0;
    if(!enif_get_string_length(env, argv[1], &clientNameLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_client_name");
    }

    char* clientName = (char*)malloc(clientNameLength + 1);
    if(!enif_get_string(env, argv[1], clientName, clientNameLength + 1, ERL_NIF_UTF8))
    {
        free(clientName);
        return enif_make_atom(env, "invalid_client_name");
    }

    // Get participant
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[2], PARTICIPANT_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_participant");
    }

    ParticipantStruct* participantStruct = (ParticipantStruct*)voidStruct;

    // Get return PID
    ErlNifPid* listenerPID = (ErlNifPid*)malloc(sizeof(ErlNifPid*));
    if(!enif_get_local_pid(env, argv[3], listenerPID))
    {
        return enif_make_atom(env, "invalid_listener_pid");
    }

    // Get QoS profile
    unsigned int qosParamLength = 0;
    if(!enif_get_string_length(env, argv[4], &qosParamLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_profile");
    }

    bool useQos = false;
    char* qosParam = nullptr;
    if(qosParamLength > 0)
    {
        useQos = true;
        qosParam = (char*)malloc(qosParamLength + 1);
        if(!enif_get_string(env, argv[4], qosParam, qosParamLength + 1, ERL_NIF_UTF8))
        {
            free(qosParam);
            return enif_make_atom(env, "invalid_qos_profile");
        }
    }

    // Find topic if it exists
    eprosima::fastdds::dds::Topic* topic = participantStruct->participant->find_topic(topicName, 1);
    
    // Topic doesn't exist, make it
    if(topic == nullptr)
    {
        topic = participantStruct->participant->create_topic_with_profile(topicName, PSMark::DeviceMessagePubSubType().get_name(), qosParam);
    }

    if(topic == nullptr)
    {
       return enif_make_atom(env, "topic_creation_failed");
    }
    
    // Create the subscriber
    eprosima::fastdds::dds::Subscriber* subscriber = participantStruct->participant->create_subscriber_with_profile(qosParam);
    if(subscriber == nullptr)
    {
        return enif_make_atom(env, "subscriber_creation_failed");
    }

    // Create the datareader
    FastDDSDataReaderListenerImpl* listener = new FastDDSDataReaderListenerImpl(listenerPID, clientName, topicName, participantStruct->participant);
    eprosima::fastdds::dds::DataReader* datareader = subscriber->create_datareader_with_profile(topic, qosParam, listener);
    if(datareader == nullptr)
    {
        return enif_make_atom(env, "datareader_creation_failed");
    }

    SubscriberStruct* res;
    ERL_NIF_TERM ret;

    res = (SubscriberStruct*) enif_alloc_resource(SUBSCRIBER_RES_TYPE, sizeof(SubscriberStruct));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->subscriber = subscriber;

    return enif_make_tuple2(env, atom_ok, ret);
}

ERL_NIF_TERM create_publisher_on_topic(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    PublisherStruct* res;
    ERL_NIF_TERM ret;

    // Get topic
    unsigned int topicLength = 0;
    if(!enif_get_string_length(env, argv[0], &topicLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_topic");
    }

    char* topicName = (char*)malloc(topicLength + 1);
    if(!enif_get_string(env, argv[0], topicName, topicLength + 1, ERL_NIF_UTF8))
    {
        free(topicName);
        return enif_make_atom(env, "invalid_topic");
    }

    // Get participant
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[1], PARTICIPANT_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_participant");
    }

    ParticipantStruct* participantStruct = (ParticipantStruct*)voidStruct;

    // Get QoS profile
    unsigned int qosParamLength = 0;
    if(!enif_get_string_length(env, argv[2], &qosParamLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_profile");
    }

    // Get publishing node ID
    unsigned int publishingNodeLength = 0;
    if(!enif_get_string_length(env, argv[3], &publishingNodeLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_node_id");
    }

    char* publishingNode = (char*)malloc(publishingNodeLength + 1);
    if(!enif_get_string(env, argv[3], publishingNode, publishingNodeLength + 1, ERL_NIF_UTF8))
    {
        free(publishingNode);
        return enif_make_atom(env, "invalid_node_id");
    }

    bool useQos = false;
    char* qosParam = nullptr;
    if(qosParamLength > 0)
    {
        useQos = true;
        qosParam = (char*)malloc(qosParamLength + 1);
        if(!enif_get_string(env, argv[2], qosParam, qosParamLength + 1, ERL_NIF_UTF8))
        {
            free(qosParam);
            return enif_make_atom(env, "invalid_qos_profile");
        }
    }

     // Find topic if it exists
    eprosima::fastdds::dds::Topic* topic = participantStruct->participant->find_topic(topicName, 1);
    
    // Topic doesn't exist, make it
    if(topic == nullptr)
    {
       topic = participantStruct->participant->create_topic_with_profile(topicName, PSMark::DeviceMessagePubSubType().get_name(), qosParam);
    }

    if(topic == nullptr)
    {
       return enif_make_atom(env, "topic_creation_failed");
    }

    // Create the publisher
    eprosima::fastdds::dds::Publisher* publisher = participantStruct->participant->create_publisher_with_profile(qosParam);
    if(publisher == nullptr)
    {
        return enif_make_atom(env, "publisher_creation_failed");
    }

    // Create the datareader
    eprosima::fastdds::dds::DataWriter* datawriter = publisher->create_datawriter_with_profile(topic, qosParam);
    if(datawriter == nullptr)
    {
        return enif_make_atom(env, "datawriter_creation_failed");
    }

    // Create a container to hold the data to publish
    void* dataContainer = PSMark::DeviceMessagePubSubType().create_data();

    res = (PublisherStruct*) enif_alloc_resource(PUBLISHER_RES_TYPE, sizeof(PublisherStruct));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->publisher = publisher;
    res->writer = datawriter;
    res->nodeId = publishingNode;
    res->dataContainer = dataContainer;

    return enif_make_tuple2(env, atom_ok, ret);
}

ERL_NIF_TERM delete_publisher(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    // Get participant
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[0], PARTICIPANT_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_participant");
    }

    ParticipantStruct* participantStruct = (ParticipantStruct*)voidStruct;

    // Get publisher
    voidStruct = nullptr;
    if(!enif_get_resource(env, argv[1], PUBLISHER_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_publisher");
    }

    PublisherStruct* publisherStruct = (PublisherStruct*)voidStruct;

    if(publisherStruct->publisher->delete_contained_entities() != eprosima::fastdds::dds::RETCODE_OK)
    {
        return enif_make_atom(env, "failed_to_delete_publisher_entities");
    }

    if(participantStruct->participant->delete_publisher(publisherStruct->publisher) != eprosima::fastdds::dds::RETCODE_OK)
    {
        return enif_make_atom(env, "failed_to_delete_publisher");
    }

    publisherStruct->publisher = 0;
    publisherStruct->writer = 0;
    free(publisherStruct->nodeId);
    publisherStruct->nodeId = 0;

    return atom_ok;
}

ERL_NIF_TERM delete_subscriber(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    // Get participant
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[0], PARTICIPANT_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_participant");
    }

    ParticipantStruct* participantStruct = (ParticipantStruct*)voidStruct;

    // Get subscriber
    voidStruct = nullptr;
    if(!enif_get_resource(env, argv[1], SUBSCRIBER_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_subscriber");
    }

    SubscriberStruct* subscriberStruct = (SubscriberStruct*)voidStruct;

    if(subscriberStruct->subscriber->delete_contained_entities() != eprosima::fastdds::dds::RETCODE_OK)
    {
        return enif_make_atom(env, "failed_to_delete_subscriber_entities");
    }

    // Remove subscriber from participant
    if(participantStruct->participant->delete_subscriber(subscriberStruct->subscriber) != eprosima::fastdds::dds::RETCODE_OK)
    {
        return enif_make_atom(env, "failed_to_delete_subscriber");
    }

    subscriberStruct->subscriber = 0;

    return atom_ok;
}


ERL_NIF_TERM publish_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    // Get message sequence Id
    int seqId = 0;
    if(!enif_get_int(env, argv[0], &seqId))
    {
        return enif_make_badarg(env);
    }

    // Get message contents
    ErlNifBinary messageData;
    if(!enif_inspect_binary(env, argv[1], &messageData))
    {
        return enif_make_atom(env, "invalid_message");
    }

    // Get datawriter
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[2], PUBLISHER_RES_TYPE, &voidStruct))
    {
        return atom_not_connected;
    }

    PublisherStruct* publisherStruct = (PublisherStruct*)voidStruct;

    if(publisherStruct->publisher != 0)
    {
        PSMark::DeviceMessage* message = (PSMark::DeviceMessage*)publisherStruct->dataContainer;
        message->seq_id(seqId);
        message->publisher_id(strdup(publisherStruct->nodeId));

        std::vector<uint8_t> payloadVector(messageData.size);
        for(int i = 0; i < messageData.size; i++)
        {
            payloadVector[i] = messageData.data[i];
        }
        message->payload(payloadVector);

        if(publisherStruct->writer->write((void*)message) != eprosima::fastdds::dds::RETCODE_OK)
            return enif_make_atom(env, "write_failed");

        return atom_ok;
    }
    else
    {
        return atom_not_connected;
    }
}

static ErlNifFunc nif_funcs[] = {
    {"create_participant", 3, create_participant},
    {"create_subscriber_on_topic", 5, create_subscriber_on_topic},
    {"create_publisher_on_topic", 4, create_publisher_on_topic},
    {"publish_message", 3, publish_message},
    {"delete_subscriber", 2, delete_subscriber},
    {"delete_publisher", 2, delete_publisher}
};

ERL_NIF_INIT(psmark_fastdds_interface, nif_funcs, &load, &reload, &upgrade, &unload);