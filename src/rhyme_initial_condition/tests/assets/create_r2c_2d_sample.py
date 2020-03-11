import h5py as h5
import numpy as np
import sys

prob_domain_dtype = [
    ('lo_i', '<i4'),
    ('lo_j', '<i4'),
    ('lo_k', '<i4'),
    ('hi_i', '<i4'),
    ('hi_j', '<i4'),
    ('hi_k', '<i4')
]


if __name__ == "__main__":
    """
    Parameters:
    sys.argv[1]: path to a r2c_2d snapshot
    sys.argv[2]: resolution of the snapshot (e.g. 2048)
    sys.argv[3]: required resolution (e.g. 128)
    """

    if (len(sys.argv) < 3):
        print('USAGE: python create_r2c_2d_sample.py /path/to/r2c_2d/snap'
              ' <resolution:2048> <req-resolution:128>')
        sys.exit()

    r2c_2d_path = str(sys.argv[1])
    res = int(sys.argv[2])
    req_res = int(sys.argv[3])

    f = h5.File(r2c_2d_path, 'r')
    s = h5.File('./r2c_2d_sample.hdf5', 'w')

    for attr in f.attrs:
        print('attribute: /' + attr + ' => ' + str(f.attrs.get(attr)))
        s.attrs.create(attr, f.attrs.get(attr),
                       shape=f.attrs[attr].shape, dtype=f.attrs[attr].dtype)

    s.attrs['ProblemDomain'] = np.array([req_res, req_res, req_res])

    c = f['Chombo_global']
    cs = s.create_group('Chombo_global')

    for attr in f['Chombo_global'].attrs:
        print('attribute: /Chombo_global/' +
              attr + ' => ' + str(f.attrs.get(attr)))
        cs.attrs.create(attr, c.attrs.get(attr),
                        c.attrs[attr].shape, c.attrs[attr].dtype)

    l = f['level_0']
    sl = s.create_group('level_0')

    for attr in f['level_0'].attrs:
        print('attribute: /level_0/' + attr + ' => ' + str(f.attrs.get(attr)))
        sl.attrs.create(attr, l.attrs.get(attr),
                        l.attrs[attr].shape, l.attrs[attr].dtype)

    s['level_0'].attrs['prob_domain'] = np.array(
        [(0, 0, 0, req_res-1, req_res-1, req_res-1)],
        dtype=prob_domain_dtype
    )
    s['level_0'].attrs['dx'] = np.array([1.0 / req_res])

    sl.create_dataset('GridID', shape=(1,), dtype=np.int32, data=np.array([1]))
    sl.create_dataset('Hier_Down', shape=(
        1,), dtype=np.int32, data=np.array([-1]))
    sl.create_dataset('Hier_Next', shape=(
        1,), dtype=np.int32, data=np.array([-1]))
    sl.create_dataset('Hier_Up', shape=(1,), dtype=np.int32,
                      data=np.array([req_res]))
    sl.create_dataset('boxes', shape=(1,), dtype=prob_domain_dtype,
                      data=np.array([(0, 0, 0, req_res-1, req_res-1, 0)],
                                    dtype=prob_domain_dtype))

    d = np.zeros(6 * req_res**2)

    for i in range(6):
        dlb = i * req_res**2
        dub = (i + 1) * req_res**2
        datalb = int(i * res**2 + res*res / 2)
        dataub = int(i * res**2 + res * res / 2 + req_res**2)
        d[dlb:dub] = l['data:datatype=0'][datalb:dataub]

    sl.create_dataset('data:datatype=0', shape=(6 * req_res**2,),
                      dtype=np.float32, data=d)

    f.close
    s.close
