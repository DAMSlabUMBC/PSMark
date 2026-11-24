find_package(OpenDDS REQUIRED)

add_library(psmark_opendds_idl)
target_link_libraries(psmark_opendds_idl OpenDDS::Dcps OpenDDS::Rtps OpenDDS::Rtps_Udp)
OPENDDS_TARGET_SOURCES(psmark_opendds_idl PUBLIC "OpenDDSPSMark.idl")