
.pdb$execute_query("CREATE INDEX bsn_idx ON pseudodata.bzsprsq00 (prsburgerservicenummer);")
.pdb$execute_query("CREATE INDEX pc_idx ON pseudodata.bzsprsq00 (vblpostcode);")
.pdb$execute_query("CREATE INDEX anr_idx ON pseudodata.bzsprsq00 (prsanummer);")
.pdb$execute_query("CREATE INDEX hnr_idx ON pseudodata.bzsprsq00 (vblhuisnummer);")
.pdb$execute_query("CREATE INDEX hlet_idx ON pseudodata.bzsprsq00 (vblhuisletter);")
.pdb$execute_query("CREATE INDEX hnrt_idx ON pseudodata.bzsprsq00 (vblhuisnummertoevoeging);")


.pdb$execute_query("CREATE INDEX bsnhuw_idx ON pseudodata.bzshuwq00 (huwburgerservicenummer);")


.pdb$execute_query("CREATE INDEX bsnkin_idx ON pseudodata.bzskinq00 (prsburgerservicenummer);")
.pdb$execute_query("CREATE INDEX ON pseudodata.bzskinq00 (prsanummer);")

.pdb$execute_query("CREATE INDEX bsnsui_idx ON pseudodata.suite (bsn);")
.pdb$execute_query("CREATE INDEX bsnopw_idx ON pseudodata.openwave (bsn_nummer);")
.pdb$execute_query("CREATE INDEX bsncar_idx ON pseudodata.carel (bsn);")
.pdb$execute_query("CREATE INDEX bsnall_idx ON pseudodata.allegro (bsn);")
.pdb$execute_query("CREATE INDEX ON pseudodata.menscentraal (klant_bsn);")

.pdb$execute_query("CREATE INDEX ON pseudodata.bzsc58q00 (prsburgerservicenummer);")
.pdb$execute_query("CREATE INDEX ON pseudodata.bzsc58q00 (prsanummer);")

