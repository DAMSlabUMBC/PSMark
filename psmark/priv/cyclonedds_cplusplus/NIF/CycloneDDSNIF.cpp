#include <dds/dds.h>
#include <dds/ddsc/dds_public_qos_provider.h>

#include <erl_nif.h>
#include <CycloneDDSPSMark.h>

#include <string>
#include <iostream>

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
    dds_entity_t participant;
    dds_qos_provider_t* provider;
} ParticipantStruct;

typedef struct 
{
    dds_entity_t publisher;
    dds_entity_t writer;
    char* nodeId;
} PublisherStruct;

typedef struct 
{
    dds_entity_t subscriber;
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
    dds_delete(participantStruct->participant);
    dds_delete_qos_provider(participantStruct->provider);
}

void publisher_free_res(ErlNifEnv* env, void* obj)
{
    UNUSED_PARAM(env);
    
    PublisherStruct* publisherStruct = (PublisherStruct*) obj;

    dds_delete(publisherStruct->publisher);
    dds_delete(publisherStruct->writer);

    if(publisherStruct->nodeId != 0)
    {
        free(publisherStruct->nodeId);
        publisherStruct->nodeId = 0;
    }
}

void subscriber_free_res(ErlNifEnv* env, void* obj)
{
    UNUSED_PARAM(env);

    SubscriberStruct* subscriberStruct = (SubscriberStruct*) obj;
    dds_delete(subscriberStruct->subscriber);
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

static void data_available(dds_entity_t reader, void *arg)
{
    void* samples[MAX_SAMPLES];
    memset(samples, 0, MAX_SAMPLES);
    dds_sample_info_t info[MAX_SAMPLES];
    int status;
    ReaderListenerData* listenerData = (ReaderListenerData*)arg;

    if(listenerData == NULL)
    {
        printf("LISTENER NOT SET\n");
        return;
    }

    // Read all samples
    int sampleCount = dds_take(reader, samples, info, MAX_SAMPLES, MAX_SAMPLES);
    if (sampleCount > 0)
    {
        for (int i = 0; i < sampleCount; i++)
        {
            if(info[i].valid_data)
            {
                dds_time_t recvTime = dds_time();
                dds_time_t pubTime = info[i].source_timestamp;
                ERL_NIF_TERM recvTimeNs = enif_make_uint64(listenerData->env, recvTime);
                ERL_NIF_TERM pubTimeNs = enif_make_uint64(listenerData->env, pubTime);
                
                PSMark_DeviceMessage* message = (PSMark_DeviceMessage*)samples[i];
                ERL_NIF_TERM payloadSize = enif_make_uint(listenerData->env, message->payload._length);
                ERL_NIF_TERM seqId = enif_make_uint64(listenerData->env, message->seq_id);
                ERL_NIF_TERM publisherId = enif_make_string(listenerData->env, message->publisher_id, ERL_NIF_UTF8);
                
                ERL_NIF_TERM data_tuple = enif_make_tuple7(listenerData->env, listenerData->name, listenerData->topicName, seqId, pubTimeNs, recvTimeNs, payloadSize, publisherId);
                ERL_NIF_TERM send_tuple = enif_make_tuple2(listenerData->env, atom_recv, data_tuple);

                enif_send(NULL, listenerData->pid, NULL, send_tuple);
            }
        }
    }
    else if (sampleCount < 0)
    {
        printf("TAKE NEXT SAMPLE FAILED\n");
    }
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

    // Get QoS path
    unsigned int qosPathLength = 0;
    if(!enif_get_string_length(env, argv[2], &qosPathLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_path");
    }

    bool useQos = false;
    char* qosPath = nullptr;
    if(qosPathLength > 0)
    {
        useQos = true;
        qosPath = (char*)malloc(qosPathLength + 1);
        if(!enif_get_string(env, argv[2], qosPath, qosPathLength + 1, ERL_NIF_UTF8))
        {
            free(qosPath);
            return enif_make_atom(env, "invalid_qos_path");
        }
    }

    // Get QoS profile
    unsigned int qosProfileLength = 0;
    if(!enif_get_string_length(env, argv[3], &qosProfileLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_profile");
    }

    char* qosProfile = nullptr;
    if(qosProfileLength > 0)
    {
        qosProfile = (char*)malloc(qosProfileLength + 1);
        if(!enif_get_string(env, argv[3], qosProfile, qosProfileLength + 1, ERL_NIF_UTF8))
        {
            free(qosProfile);
            return enif_make_atom(env, "invalid_qos_profile");
        }
    }

    // Get XML Provider
    dds_qos_provider_t *provider;
    dds_return_t dds_ret = dds_create_qos_provider(qosPath, &provider);
    if (dds_ret != DDS_RETCODE_OK) 
        return enif_make_atom(env, "malformed_qos");

    // Get domain participant QoS
    const dds_qos_t* participantQos;
    dds_ret = dds_qos_provider_get_qos(provider, DDS_PARTICIPANT_QOS, qosProfile, &participantQos);
    if(dds_ret != DDS_RETCODE_OK)
    {
        free(qosPath);
        free(qosProfile);
        return enif_make_atom(env, "participant_profile_not_found");
    }

    // Create participant
    dds_entity_t participant = dds_create_participant(domainId, participantQos, NULL);
    if(participant < 0)
        return enif_make_atom(env, "participant_failed");


    res = (ParticipantStruct*) enif_alloc_resource(PARTICIPANT_RES_TYPE, sizeof(ParticipantStruct));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->participant = participant;
    res->provider = provider;

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

    // Get QoS path
    unsigned int qosPathLength = 0;
    if(!enif_get_string_length(env, argv[4], &qosPathLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_path");
    }

    bool useQos = false;
    char* qosPath = nullptr;
    if(qosPathLength > 0)
    {
        useQos = true;
        qosPath = (char*)malloc(qosPathLength + 1);
        if(!enif_get_string(env, argv[4], qosPath, qosPathLength + 1, ERL_NIF_UTF8))
        {
            free(qosPath);
            return enif_make_atom(env, "invalid_qos_path");
        }
    }

    // Get QoS profile
    unsigned int qosProfileLength = 0;
    if(!enif_get_string_length(env, argv[5], &qosProfileLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_profile");
    }

    char* qosProfile = nullptr;
    if(qosProfileLength > 0)
    {
        qosProfile = (char*)malloc(qosProfileLength + 1);
        if(!enif_get_string(env, argv[5], qosProfile, qosProfileLength + 1, ERL_NIF_UTF8))
        {
            free(qosProfile);
            return enif_make_atom(env, "invalid_qos_profile");
        }
    }

    // Get required QoS
    const dds_qos_t* topicQos;
    const dds_qos_t* subscriberQos;
    const dds_qos_t* readerQos;

    if(useQos && participantStruct->provider != NULL)
    {
        dds_qos_provider_t* provider = participantStruct->provider;

        dds_return_t dds_ret = dds_qos_provider_get_qos(provider, DDS_TOPIC_QOS, qosProfile, &topicQos);
        if(dds_ret != DDS_RETCODE_OK)
        {
            free(qosPath);
            free(qosProfile);
            return enif_make_atom(env, "topic_profile_not_found");
        }
        
        dds_ret = dds_qos_provider_get_qos(provider, DDS_SUBSCRIBER_QOS, qosProfile, &subscriberQos);
        if(dds_ret != DDS_RETCODE_OK)
        {
            free(qosPath);
            free(qosProfile);
            return enif_make_atom(env, "subscriber_profile_not_found");
        }
        
        dds_ret = dds_qos_provider_get_qos(provider, DDS_READER_QOS, qosProfile, &readerQos);
        if(dds_ret != DDS_RETCODE_OK)
        {
            free(qosPath);
            free(qosProfile);
            return enif_make_atom(env, "datareader_profile_not_found");
        }
    }

    // Find topic if it exists
    dds_entity_t topic = dds_find_topic(DDS_FIND_SCOPE_PARTICIPANT, participantStruct->participant, topicName, NULL, 1);
    
    // Topic doesn't exist, make it
    if(topic < 0)
    {
        topic = dds_create_topic(participantStruct->participant, &PSMark_DeviceMessage_desc, topicName, topicQos, NULL);
    }

    if(topic < 0)
        return enif_make_atom(env, "topic_creation_failed");

    // Create the subscriber
    dds_entity_t subscriber = dds_create_subscriber(participantStruct->participant, subscriberQos, NULL);
    if(subscriber < 0)
        return enif_make_atom(env, "subscriber_creation_failed");

    // Create the listener
    ReaderListenerData* listenerData = (ReaderListenerData*)malloc(sizeof(ReaderListenerData));
    listenerData->pid = listenerPID;
    listenerData->env = enif_alloc_env();
    listenerData->name = enif_make_string(listenerData->env, clientName, ERL_NIF_UTF8);
    listenerData->topicName = enif_make_string(listenerData->env, topicName, ERL_NIF_UTF8);

    dds_listener_t* listener = dds_create_listener(NULL);
    dds_lset_data_available_arg(listener, data_available, listenerData, false);

    // Create the datareader
    dds_entity_t datareader = dds_create_reader(subscriber, topic, readerQos, listener);
    if(datareader < 0)
        return enif_make_atom(env, "datareader_creation_failed");

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

    // Get QoS path
    unsigned int qosPathLength = 0;
    if(!enif_get_string_length(env, argv[2], &qosPathLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_path");
    }

    bool useQos = false;
    char* qosPath = nullptr;
    if(qosPathLength > 0)
    {
        useQos = true;
        qosPath = (char*)malloc(qosPathLength + 1);
        if(!enif_get_string(env, argv[2], qosPath, qosPathLength + 1, ERL_NIF_UTF8))
        {
            free(qosPath);
            return enif_make_atom(env, "invalid_qos_path");
        }
    }

    // Get QoS profile
    unsigned int qosProfileLength = 0;
    if(!enif_get_string_length(env, argv[3], &qosProfileLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_qos_profile");
    }

    char* qosProfile = nullptr;
    if(qosProfileLength > 0)
    {
        qosProfile = (char*)malloc(qosProfileLength + 1);
        if(!enif_get_string(env, argv[3], qosProfile, qosProfileLength + 1, ERL_NIF_UTF8))
        {
            free(qosProfile);
            return enif_make_atom(env, "invalid_qos_profile");
        }
    }

    // Get publishing node ID
    unsigned int publishingNodeLength = 0;
    if(!enif_get_string_length(env, argv[4], &publishingNodeLength, ERL_NIF_UTF8))
    {
        return enif_make_atom(env, "invalid_node_id");
    }

    char* publishingNode = (char*)malloc(publishingNodeLength + 1);
    if(!enif_get_string(env, argv[4], publishingNode, publishingNodeLength + 1, ERL_NIF_UTF8))
    {
        free(publishingNode);
        return enif_make_atom(env, "invalid_node_id");
    }

     // Get required QoS
    const dds_qos_t* topicQos;
    const dds_qos_t* publisherQos;
    const dds_qos_t* writerQos;

    if(useQos && participantStruct->provider != NULL)
    {
        dds_qos_provider_t* provider = participantStruct->provider;

        dds_return_t dds_ret = dds_qos_provider_get_qos(provider, DDS_TOPIC_QOS, qosProfile, &topicQos);
        if(dds_ret != DDS_RETCODE_OK)
        {
            free(qosPath);
            free(qosProfile);
            return enif_make_atom(env, "topic_profile_not_found");
        }
        
        dds_ret = dds_qos_provider_get_qos(provider, DDS_PUBLISHER_QOS, qosProfile, &publisherQos);
        if(dds_ret != DDS_RETCODE_OK)
        {
            free(qosPath);
            free(qosProfile);
            return enif_make_atom(env, "publisher_profile_not_found");
        }
        
        dds_ret = dds_qos_provider_get_qos(provider, DDS_WRITER_QOS, qosProfile, &writerQos);
        if(dds_ret != DDS_RETCODE_OK)
        {
            free(qosPath);
            free(qosProfile);
            return enif_make_atom(env, "datawriter_profile_not_found");
        }
    }

    // Find topic if it exists
    dds_entity_t topic = dds_find_topic(DDS_FIND_SCOPE_PARTICIPANT, participantStruct->participant, topicName, NULL, 1);
    
    // Topic doesn't exist, make it
    if(topic < 0)
    {
        topic = dds_create_topic(participantStruct->participant, &PSMark_DeviceMessage_desc, topicName, topicQos, NULL);
    }

    if(topic < 0)
        return enif_make_atom(env, "topic_creation_failed");

    // Create the subscriber
    dds_entity_t publisher = dds_create_publisher(participantStruct->participant, publisherQos, NULL);
    if(publisher < 0)
        return enif_make_atom(env, "publisher_creation_failed");

    // Create the datareader
    dds_entity_t datawriter = dds_create_writer(publisher, topic, writerQos, NULL);
    if(datawriter < 0)
        return enif_make_atom(env, "datawriter_creation_failed");

    res = (PublisherStruct*) enif_alloc_resource(PUBLISHER_RES_TYPE, sizeof(PublisherStruct));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->publisher = publisher;
    res->writer = datawriter;
    res->nodeId = publishingNode;

    return enif_make_tuple2(env, atom_ok, ret);
}

ERL_NIF_TERM delete_publisher(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED_PARAM(argc);

    // Get publisher
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[1], PUBLISHER_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_publisher");
    }

    PublisherStruct* publisherStruct = (PublisherStruct*)voidStruct;
    if(dds_delete(publisherStruct->publisher) != DDS_RETCODE_OK)
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

    // Get subscriber
    void* voidStruct = nullptr;
    if(!enif_get_resource(env, argv[1], SUBSCRIBER_RES_TYPE, &voidStruct))
    {
        return enif_make_atom(env, "failed_to_fetch_subscriber");
    }

    SubscriberStruct* subscriberStruct = (SubscriberStruct*)voidStruct;

    // Remove subscriber from participant
    if(dds_delete(subscriberStruct->subscriber) != DDS_RETCODE_OK)
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
        PSMark_DeviceMessage message;
        message.seq_id = seqId;
        message.payload._length = messageData.size;
        message.payload._maximum = messageData.size;
        message.payload._buffer = dds_sequence_octet_allocbuf(messageData.size);
        memcpy(message.payload._buffer, messageData.data, messageData.size);
        message.publisher_id = strdup(publisherStruct->nodeId);

        dds_return_t dds_ret = dds_write(publisherStruct->writer, &message);
        if(dds_ret != DDS_RETCODE_OK)
            return enif_make_atom(env, "write_failed");

        return atom_ok;
    }
    else
    {
        return atom_not_connected;
    }
}

static ErlNifFunc nif_funcs[] = {
    {"create_participant", 4, create_participant},
    {"create_subscriber_on_topic", 6, create_subscriber_on_topic},
    {"create_publisher_on_topic", 5, create_publisher_on_topic},
    {"publish_message", 3, publish_message},
    {"delete_subscriber", 2, delete_subscriber},
    {"delete_publisher", 2, delete_publisher}
};

ERL_NIF_INIT(psmark_default_dds_interface, nif_funcs, &load, &reload, &upgrade, &unload);