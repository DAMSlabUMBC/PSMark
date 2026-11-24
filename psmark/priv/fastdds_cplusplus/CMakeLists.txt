add_library(psmark_fastdds)

include(ExternalProject)
ExternalProject_Add(FastDDS
        GIT_REPOSITORY https://github.com/eProsima/Fast-DDS.git
        GIT_TAG v3.6.2
        INSTALL_DIR ${INSTALL_DIR}/fastdds
        CMAKE_CACHE_ARGS
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH};${INSTALL_DIR}
        -DBUILD_SHARED_LIBS:BOOL=ON
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        -DCMAKE_TOOLCHAIN_FILE:PATH=${CMAKE_TOOLCHAIN_FILE}
        -DTHIRDPARTY:BOOL=ON  # install asio, tinyxml2 from Git
        -DSECURITY:BOOL=OFF
        -DSHM_TRANSPORT_DEFAULT:BOOL=OFF
)

include_directories(
        ${INSTALL_DIR}/fastdds/include
)

target_sources(psmark_fastdds
    PRIVATE
        DDS_IDL/FastDDSPSMarkPubSubTypes.cxx
        DDS_IDL/FastDDSPSMarkTypeObjectSupport.cxx
        NIF/FastDDSDataReaderListenerImpl.cpp
        NIF/FastDDSNIF.cpp
    
    PUBLIC
        FILE_SET HEADERS
        FILES 
            DDS_IDL/FastDDSPSMark.hpp
            DDS_IDL/FastDDSPSMarkCdrAux.hpp
            DDS_IDL/FastDDSPSMarkPubSubTypes.hpp
            DDS_IDL/FastDDSPSMarkTypeObjectSupport.hpp
            NIF/FastDDSDataReaderListenerImpl.h
)

target_include_directories(psmark_fastdds PRIVATE ${ERTS_ROOT}/include)