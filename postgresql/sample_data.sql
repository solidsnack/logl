INSERT INTO logl.log VALUES
  ( 'aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-20', '2011-03-20', ''          );
INSERT INTO logl.log VALUES
  ( 'aaaaaaa1-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-21', '2011-03-21', 'to delete' );
INSERT INTO logl.log VALUES
  ( 'aaaaaaa2-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-22', '2011-03-22', ''          );

INSERT INTO logl.entry VALUES
  ( 'aaaaaaaa-bbb0-cccc-dddd-eeeeeeeeeeee',
    'aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-20', '2011-03-21', '', 'data0' );
INSERT INTO logl.entry VALUES
  ( 'aaaaaaaa-bbb1-cccc-dddd-eeeeeeeeeeee',
    'aaaaaaa1-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-20', '2011-03-22', '', 'data1' );
INSERT INTO logl.entry VALUES
  ( 'aaaaaaaa-bbb2-cccc-dddd-eeeeeeeeeeee',
    'aaaaaaa1-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-20', '2011-03-23', '', 'data2' );
INSERT INTO logl.entry VALUES
  ( 'aaaaaaaa-bbb3-cccc-dddd-eeeeeeeeeeee',
    'aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-20', '2011-03-22', '', 'data3' );
INSERT INTO logl.entry VALUES
  ( 'aaaaaaaa-bbb4-cccc-dddd-eeeeeeeeeeee',
    'aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee',
    '2011-03-20', '2011-03-23', '', 'data4' );

INSERT INTO logl.tombstone VALUES
  ( 'aaaaaaa1-bbbb-cccc-dddd-eeeeeeeeeeee', '2011-03-24' );

