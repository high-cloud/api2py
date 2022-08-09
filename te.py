def get_p_d_g_graph_contexts(session: HDATA.Session,
                             start: int,
                             length: int)\
        -> Tuple[Array, Array]:
    """Wrapper for [session, start, length]
    ,
    Return an array of PDG graph context names and ids, the first count names will be returned. These ids can be used with HAPI_GetPDGEvents and HAPI_GetPDGState . The values of the names can be retrieved with HAPI_GetString .

    Args:
        session: The session of Houdini you are interacting with. See HAPI_Sessions for more on sessions. Pass NULL to just use the default in-process session.
        start: First index of range. Must be at least 0 and at most context_count - 1 where context_count is the count returned by HAPI_GetPDGGraphContextsCount()
        length: Given num_contexts returned by HAPI_GetPDGGraphContextsCount() , length should be at least 0 and at most num_contexts - start.

    Returns:
        Array of context names stored as ::HAPI_StringHandle at least the size of length. These can be used with HAPI_GetString() and are valid until the next call to this function.
        Array of graph context ids at least the size of length.
    """



    context_names_array = (c_int * length)()
    context_id_array = (c_int * length)()
    result = HAPI_LAB.HAPI_GetPDGGraphContexts(byref(session), byref(context_names_array), byref(context_id_array), c_int(start), c_int(length))
    assert HDATA.Result.SUCCESS == result, 'GetPDGGraphContexts Failed with {0}'.format(HDATA.Result(result).name)
    return context_names_array, context_id_array


def get_p_d_g_graph_contexts_count(session: HDATA.Session)\
        -> int:
    """Wrapper for [session]
    ,
    Return the total number of PDG graph contexts found.

    Args:
        session: The session of Houdini you are interacting with. See HAPI_Sessions for more on sessions. Pass NULL to just use the default in-process session.

    Returns:
        Total PDG graph contexts count.
    """



    result = HAPI_LAB.HAPI_GetPDGGraphContextsCount(byref(session), byref(num_contexts))
    assert HDATA.Result.SUCCESS == result, 'GetPDGGraphContextsCount Failed with {0}'.format(HDATA.Result(result).name)
    return num_contexts