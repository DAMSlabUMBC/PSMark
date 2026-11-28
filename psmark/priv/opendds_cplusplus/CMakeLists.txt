find_package(OpenDDS REQUIRED)

set(ENV{LD_LIBRARY_PATH} $ENV{LD_LIBRARY_PATH}:${OPENDDS_ROOT}/lib)

add_library(psmark_opendds SHARED)
opendds_target_sources(psmark_opendds PUBLIC "DDS_IDL/OpenDDSPSMark.idl")
target_include_directories(psmark_opendds PRIVATE ${ERTS_ROOT}/include)

target_sources(psmark_opendds
    PRIVATE
        NIF/OpenDDSDataReaderListenerImpl.cpp
        NIF/OpenDDSNIF.cpp

    PUBLIC
        FILE_SET HEADERS
        FILES 
            NIF/OpenDDSDataReaderListenerImpl.h
)

find_library(XERCES-C xerces-c)
target_link_libraries(psmark_opendds PRIVATE OpenDDS::Dcps OpenDDS::Rtps OpenDDS::Rtps_Udp OpenDDS::QOS_XML_XSC_Handler ${XERCES-C})