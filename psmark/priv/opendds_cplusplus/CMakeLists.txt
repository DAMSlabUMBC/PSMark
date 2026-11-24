find_package(OpenDDS REQUIRED)

set(ENV{LD_LIBRARY_PATH} $ENV{LD_LIBRARY_PATH}:${OPENDDS_ROOT}/lib)

add_library(psmark_opendds_idl)
target_link_libraries(psmark_opendds_idl OpenDDS::Dcps OpenDDS::Rtps OpenDDS::Rtps_Udp)
opendds_target_sources(psmark_opendds_idl PUBLIC "DDS_IDL/OpenDDSPSMark.idl")

add_library(psmark_opendds)
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

target_link_libraries(psmark_opendds PRIVATE OpenDDS::Dcps OpenDDS::Rtps OpenDDS::Rtps_Udp psmark_opendds_idl)