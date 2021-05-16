#~# Calculate EOO per species per country----
# Data MUST be in the form: decimal latitude, decimal longitude, name
# Tenodera sinensis
Ts <- sppOcc.min %>% filter(scientificName == "Tenodera sinensis")
Ts.EOO <- data.frame(ddlat = Ts$decimalLatitude,
                     ddlon = Ts$decimalLongitude, 
                     country = Ts$country,
                     name = "Tenodera sinensis")
Ts.EOO <- Ts.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ts.EOO.res <- EOO.computing(Ts.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ts.EOO.res.c <- row.names(Ts.EOO.res)
Ts.EOO.res <- data.frame(Ts.EOO.res, country = Ts.EOO.res.c)
Ts.EOO <- merge(Ts.EOO, Ts.EOO.res, by.x = "country", by.y = "country")

# Cales noacki
Cn <- sppOcc.min %>% filter(scientificName == "Cales noacki")
Cn.EOO <- data.frame(ddlat = Cn$decimalLatitude,
                     ddlon = Cn$decimalLongitude, 
                     country = Cn$country,
                     name = "Cales noacki")
Cn.EOO <- Cn.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Cn.EOO.res <- EOO.computing(Cn.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Cn.EOO.res.c <- row.names(Cn.EOO.res)
Cn.EOO.res <- data.frame(Cn.EOO.res, country = Cn.EOO.res.c)
Cn.EOO <- merge(Cn.EOO, Cn.EOO.res, by.x = "country", by.y = "country")

# Apis mellifera carnica
Amc <- sppOcc.min %>% filter(scientificName == "Apis mellifera carnica")
Amc.EOO <- data.frame(ddlat = Amc$decimalLatitude,
                      ddlon = Amc$decimalLongitude, 
                      country = Amc$country,
                      name = "Apis mellifera carnica")
Amc.EOO <- Amc.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Amc.EOO.res <- EOO.computing(Amc.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Amc.EOO.res.c <- row.names(Amc.EOO.res)
Amc.EOO.res <- data.frame(Amc.EOO.res, country = Amc.EOO.res.c)
Amc.EOO <- merge(Amc.EOO, Amc.EOO.res, by.x = "country", by.y = "country")

# Bombus hortorum
Bh <- sppOcc.min %>% filter(scientificName == "Bombus hortorum")
Bh.EOO <- data.frame(ddlat = Bh$decimalLatitude,
                     ddlon = Bh$decimalLongitude, 
                     country = Bh$country,
                     name = "Bombus hortorum")
Bh.EOO <- Bh.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bh.EOO.res <- EOO.computing(Bh.EOO, method.range = "alpha.hull", method.less.than3 = "arbitrary")
Bh.EOO.res.c <- row.names(Bh.EOO.res)
Bh.EOO.res <- data.frame(Bh.EOO.res, country = Bh.EOO.res.c)
Bh.EOO <- merge(Bh.EOO, Bh.EOO.res, by.x = "country", by.y = "country")

# Bombus impatiens
Bi <- sppOcc.min %>% filter(scientificName == "Bombus impatiens")
Bi.EOO <- data.frame(ddlat = Bi$decimalLatitude,
                     ddlon = Bi$decimalLongitude, 
                     country = Bi$country,
                     name = "Bombus impatiens")
Bi.EOO <- Bi.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bi.EOO.res <- EOO.computing(Bi.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Bi.EOO.res.c <- row.names(Bi.EOO.res)
Bi.EOO.res <- data.frame(Bi.EOO.res, country = Bi.EOO.res.c)
Bi.EOO <- merge(Bi.EOO, Bi.EOO.res, by.x = "country", by.y = "country")

# Bombus lucorum
Bl <- sppOcc.min %>% filter(scientificName == "Bombus lucorum")
Bl.EOO <- data.frame(ddlat = Bl$decimalLatitude,
                     ddlon = Bl$decimalLongitude, 
                     country = Bl$country,
                     name = "Bombus lucorum")
Bl.EOO <- Bl.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bl.EOO.res <- EOO.computing(Bl.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Bl.EOO.res.c <- row.names(Bl.EOO.res)
Bl.EOO.res <- data.frame(Bl.EOO.res, country = Bl.EOO.res.c)
Bl.EOO <- merge(Bl.EOO, Bl.EOO.res, by.x = "country", by.y = "country")

# Bombus pascuorum
Bp <- sppOcc.min %>% filter(scientificName == "Bombus pascuorum")
Bp.EOO <- data.frame(ddlat = Bp$decimalLatitude,
                     ddlon = Bp$decimalLongitude, 
                     country = Bp$country,
                     name = "Bombus pascuorum")
Bp.EOO <- Bp.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bp.EOO.res <- EOO.computing(Bp.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Bp.EOO.res.c <- row.names(Bp.EOO.res)
Bp.EOO.res <- data.frame(Bp.EOO.res, country = Bp.EOO.res.c)
Bp.EOO <- merge(Bp.EOO, Bp.EOO.res, by.x = "country", by.y = "country")

# Bombus ruderatus
Br <- sppOcc.min %>% filter(scientificName == "Bombus ruderatus")
Br.EOO <- data.frame(ddlat = Br$decimalLatitude,
                     ddlon = Br$decimalLongitude, 
                     country = Br$country,
                     name = "Bombus ruderatus")
Br.EOO <- Br.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Br.EOO.res <- EOO.computing(Br.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Br.EOO.res.c <- row.names(Br.EOO.res)
Br.EOO.res <- data.frame(Br.EOO.res, country = Br.EOO.res.c)
Br.EOO <- merge(Br.EOO, Br.EOO.res, by.x = "country", by.y = "country")

# Aproceros leucopoda
Al <- sppOcc.min %>% filter(scientificName == "Aproceros leucopoda")
Al.EOO <- data.frame(ddlat = Al$decimalLatitude,
                     ddlon = Al$decimalLongitude, 
                     country = Al$country,
                     name = "Aproceros leucopoda")
Al.EOO <- Al.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Al.EOO.res <- EOO.computing(Al.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Al.EOO.res.c <- row.names(Al.EOO.res)
Al.EOO.res <- data.frame(Al.EOO.res, country = Al.EOO.res.c)
Al.EOO <- merge(Al.EOO, Al.EOO.res, by.x = "country", by.y = "country")


# Dryocosmus kuriphilus
Dk <- sppOcc.min %>% filter(scientificName == "Dryocosmus kuriphilus")
Dk.EOO <- data.frame(ddlat = Dk$decimalLatitude,
                     ddlon = Dk$decimalLongitude, 
                     country = Dk$country,
                     name = "Dryocosmus kuriphilus")
Dk.EOO <- Dk.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Dk.EOO.res <- EOO.computing(Dk.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Dk.EOO.res.c <- row.names(Dk.EOO.res)
Dk.EOO.res <- data.frame(Dk.EOO.res, country = Dk.EOO.res.c)
Dk.EOO <- merge(Dk.EOO, Dk.EOO.res, by.x = "country", by.y = "country")

# Diprion similis
Ds <- sppOcc.min %>% filter(scientificName == "Diprion similis")
Ds.EOO <- data.frame(ddlat = Ds$decimalLatitude,
                     ddlon = Ds$decimalLongitude, 
                     country = Ds$country,
                     name = "Diprion similis")
Ds.EOO <- Ds.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ds.EOO.res <- EOO.computing(Ds.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ds.EOO.res.c <- row.names(Ds.EOO.res)
Ds.EOO.res <- data.frame(Ds.EOO.res, country = Ds.EOO.res.c)
Ds.EOO <- merge(Ds.EOO, Ds.EOO.res, by.x = "country", by.y = "country")

# Adelges abietis
Aa <- sppOcc.min %>% filter(scientificName == "Adelges abietis")
Aa.EOO <- data.frame(ddlat = Aa$decimalLatitude,
                     ddlon = Aa$decimalLongitude, 
                     country = Aa$country,
                     name = "Adelges abietis")
Aa.EOO <- Aa.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Aa.EOO.res <- EOO.computing(Aa.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Aa.EOO.res.c <- row.names(Aa.EOO.res)
Aa.EOO.res <- data.frame(Aa.EOO.res, country = Aa.EOO.res.c)
Aa.EOO <- merge(Aa.EOO, Aa.EOO.res, by.x = "country", by.y = "country")

# Aethina tumida
At <- sppOcc.min %>% filter(scientificName == "Aethina tumida")
At.EOO <- data.frame(ddlat = At$decimalLatitude,
                     ddlon = At$decimalLongitude, 
                     country = At$country,
                     name = "Aethina tumida")
At.EOO <- At.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
At.EOO.res <- EOO.computing(At.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
At.EOO.res.c <- row.names(At.EOO.res)
At.EOO.res <- data.frame(At.EOO.res, country = At.EOO.res.c)
At.EOO <- merge(At.EOO, At.EOO.res, by.x = "country", by.y = "country")

# Mantis religiosa
Mr <- sppOcc.min %>% filter(scientificName == "Mantis religiosa")
Mr.EOO <- data.frame(ddlat = Mr$decimalLatitude,
                     ddlon = Mr$decimalLongitude, 
                     country = Mr$country,
                     name = "Mantis religiosa")
Mr.EOO <- Mr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mr.EOO.res <- EOO.computing(Mr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mr.EOO.res.c <- row.names(Mr.EOO.res)
Mr.EOO.res <- data.frame(Mr.EOO.res, country = Mr.EOO.res.c)
Mr.EOO <- merge(Mr.EOO, Mr.EOO.res, by.x = "country", by.y = "country")

# Acantholyda erythrocephala
Ae <- sppOcc.min %>% filter(scientificName == "Acantholyda erythrocephala")
Ae.EOO <- data.frame(ddlat = Ae$decimalLatitude,
                     ddlon = Ae$decimalLongitude, 
                     country = Ae$country,
                     name = "Acantholyda erythrocephala")
Ae.EOO <- Ae.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ae.EOO.res <- EOO.computing(Ae.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ae.EOO.res.c <- row.names(Ae.EOO.res)
Ae.EOO.res <- data.frame(Ae.EOO.res, country = Ae.EOO.res.c)
Ae.EOO <- merge(Ae.EOO, Ae.EOO.res, by.x = "country", by.y = "country")

# Adelges cooleyi
Ac <- sppOcc.min %>% filter(scientificName == "Adelges cooleyi")
Ac.EOO <- data.frame(ddlat = Ac$decimalLatitude,
                     ddlon = Ac$decimalLongitude, 
                     country = Ac$country,
                     name = "Adelges cooleyi")
Ac.EOO <- Ac.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ac.EOO.res <- EOO.computing(Ac.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ac.EOO.res.c <- row.names(Ac.EOO.res)
Ac.EOO.res <- data.frame(Ac.EOO.res, country = Ac.EOO.res.c)
Ac.EOO <- merge(Ac.EOO, Ac.EOO.res, by.x = "country", by.y = "country")

# Coptotermes gestroi
Cg <- sppOcc.min %>% filter(scientificName == "Coptotermes gestroi")
Cg.EOO <- data.frame(ddlat = Cg$decimalLatitude,
                     ddlon = Cg$decimalLongitude, 
                     country = Cg$country,
                     name = "Coptotermes gestroi")
Cg.EOO <- Cg.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Cg.EOO.res <- EOO.computing(Cg.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Cg.EOO.res.c <- row.names(Cg.EOO.res)
Cg.EOO.res <- data.frame(Cg.EOO.res, country = Cg.EOO.res.c)
Cg.EOO <- merge(Cg.EOO, Cg.EOO.res, by.x = "country", by.y = "country")

# Forficula auricularia 
Fa <- sppOcc.min %>% filter(scientificName == "Forficula auricularia")
Fa.EOO <- data.frame(ddlat = Fa$decimalLatitude,
                     ddlon = Fa$decimalLongitude, 
                     country = Fa$country,
                     name = "Forficula auricularia")
Fa.EOO <- Fa.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Fa.EOO.res <- EOO.computing(Fa.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Fa.EOO.res.c <- row.names(Fa.EOO.res)
Fa.EOO.res <- data.frame(Fa.EOO.res, country = Fa.EOO.res.c)
Fa.EOO <- merge(Fa.EOO, Fa.EOO.res, by.x = "country", by.y = "country")

# Leptopsylla segnis
Ls <- sppOcc.min %>% filter(scientificName == "Leptopsylla segnis")
Ls.EOO <- data.frame(ddlat = Ls$decimalLatitude,
                     ddlon = Ls$decimalLongitude, 
                     country = Ls$country,
                     name = "Leptopsylla segnis")
Ls.EOO <- Ls.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ls.EOO.res <- EOO.computing(Ls.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ls.EOO.res.c <- row.names(Ls.EOO.res)
Ls.EOO.res <- data.frame(Ls.EOO.res, country = Ls.EOO.res.c)
Ls.EOO <- merge(Ls.EOO, Ls.EOO.res, by.x = "country", by.y = "country")

# Afranthidium repetitum
Ar <- sppOcc.min %>% filter(scientificName == "Afranthidium repetitum")
Ar.EOO <- data.frame(ddlat = Ar$decimalLatitude,
                     ddlon = Ar$decimalLongitude, 
                     country = Ar$country,
                     name = "Afranthidium repetitum")
Ar.EOO <- Ar.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ar.EOO.res <- EOO.computing(Ar.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ar.EOO.res.c <- row.names(Ar.EOO.res)
Ar.EOO.res <- data.frame(Ar.EOO.res, country = Ar.EOO.res.c)
Ar.EOO <- merge(Ar.EOO, Ar.EOO.res, by.x = "country", by.y = "country")

# Periplaneta americana
Pam <- sppOcc.min %>% filter(scientificName == "Periplaneta americana")
Pam.EOO <- data.frame(ddlat = Pam$decimalLatitude,
                      ddlon = Pam$decimalLongitude, 
                      country = Pam$country,
                      name = "Periplaneta americana")
Pam.EOO <- Pam.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pam.EOO.res <- EOO.computing(Pam.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pam.EOO.res.c <- row.names(Pam.EOO.res)
Pam.EOO.res <- data.frame(Pam.EOO.res, country = Pam.EOO.res.c)
Pam.EOO <- merge(Pam.EOO, Pam.EOO.res, by.x = "country", by.y = "country")

# Periplaneta australasiae
Pau <- sppOcc.min %>% filter(scientificName == "Periplaneta australasiae")
Pau.EOO <- data.frame(ddlat = Pau$decimalLatitude,
                      ddlon = Pau$decimalLongitude, 
                      country = Pau$country,
                      name = "Periplaneta australasiae")
Pau.EOO <- Pau.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pau.EOO.res <- EOO.computing(Pau.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pau.EOO.res.c <- row.names(Pau.EOO.res)
Pau.EOO.res <- data.frame(Pau.EOO.res, country = Pau.EOO.res.c)
Pau.EOO <- merge(Pau.EOO, Pau.EOO.res, by.x = "country", by.y = "country")

# Reticulitermes flavipes
Rf <- sppOcc.min %>% filter(scientificName == "Reticulitermes flavipes")
Rf.EOO <- data.frame(ddlat = Rf$decimalLatitude,
                     ddlon = Rf$decimalLongitude, 
                     country = Rf$country,
                     name = "Reticulitermes flavipes")
Rf.EOO <- Rf.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Rf.EOO.res <- EOO.computing(Rf.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Rf.EOO.res.c <- row.names(Rf.EOO.res)
Rf.EOO.res <- data.frame(Rf.EOO.res, country = Rf.EOO.res.c)
Rf.EOO <- merge(Rf.EOO, Rf.EOO.res, by.x = "country", by.y = "country")


# Taeniothrips inconsequens
Ti <- sppOcc.min %>% filter(scientificName == "Taeniothrips inconsequens")
Ti.EOO <- data.frame(ddlat = Ti$decimalLatitude,
                     ddlon = Ti$decimalLongitude, 
                     country = Ti$country,
                     name = "Taeniothrips inconsequens")
Ti.EOO <- Ti.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ti.EOO.res <- EOO.computing(Ti.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ti.EOO.res.c <- row.names(Ti.EOO.res)
Ti.EOO.res <- data.frame(Ti.EOO.res, country = Ti.EOO.res.c)
Ti.EOO <- merge(Ti.EOO, Ti.EOO.res, by.x = "country", by.y = "country")

# Rhinocyllus conicus
Rc <- sppOcc.min %>% filter(scientificName == "Rhinocyllus conicus")
Rc.EOO <- data.frame(ddlat = Rc$decimalLatitude,
                     ddlon = Rc$decimalLongitude, 
                     country = Rc$country,
                     name = "Rhinocyllus conicus")
Rc.EOO <- Rc.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Rc.EOO.res <- EOO.computing(Rc.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Rc.EOO.res.c <- row.names(Rc.EOO.res)
Rc.EOO.res <- data.frame(Rc.EOO.res, country = Rc.EOO.res.c)
Rc.EOO <- merge(Rc.EOO, Rc.EOO.res, by.x = "country", by.y = "country")

# Obolodiplosis robiniae
Or <- sppOcc.min %>% filter(scientificName == "Obolodiplosis robiniae")
Or.EOO <- data.frame(ddlat = Or$decimalLatitude,
                     ddlon = Or$decimalLongitude, 
                     country = Or$country,
                     name = "Obolodiplosis robiniae")
Or.EOO <- Or.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Or.EOO.res <- EOO.computing(Or.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Or.EOO.res.c <- row.names(Or.EOO.res)
Or.EOO.res <- data.frame(Or.EOO.res, country = Or.EOO.res.c)
Or.EOO <- merge(Or.EOO, Or.EOO.res, by.x = "country", by.y = "country")

# Uroleucon nigrotuberculatum
Un <- sppOcc.min %>% filter(scientificName == "Uroleucon nigrotuberculatum")
Un.EOO <- data.frame(ddlat = Un$decimalLatitude,
                     ddlon = Un$decimalLongitude, 
                     country = Un$country,
                     name = "Uroleucon nigrotuberculatum")
Un.EOO <- Un.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Un.EOO.res <- EOO.computing(Un.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Un.EOO.res.c <- row.names(Un.EOO.res)
Un.EOO.res <- data.frame(Un.EOO.res, country = Un.EOO.res.c)
Un.EOO <- merge(Un.EOO, Un.EOO.res, by.x = "country", by.y = "country")

# Toumeyella parvicornis
Tp <- sppOcc.min %>% filter(scientificName == "Toumeyella parvicornis")
Tp.EOO <- data.frame(ddlat = Tp$decimalLatitude,
                     ddlon = Tp$decimalLongitude, 
                     country = Tp$country,
                     name = "Toumeyella parvicornis")
Tp.EOO <- Tp.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tp.EOO.res <- EOO.computing(Tp.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tp.EOO.res.c <- row.names(Tp.EOO.res)
Tp.EOO.res <- data.frame(Tp.EOO.res, country = Tp.EOO.res.c)
Tp.EOO <- merge(Tp.EOO, Tp.EOO.res, by.x = "country", by.y = "country")

# Insignorthezia insignis
Ii <- sppOcc.min %>% filter(scientificName == "Insignorthezia insignis")
Ii.EOO <- data.frame(ddlat = Ii$decimalLatitude,
                     ddlon = Ii$decimalLongitude, 
                     country = Ii$country,
                     name = "Insignorthezia insignis")
Ii.EOO <- Ii.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ii.EOO.res <- EOO.computing(Ii.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ii.EOO.res.c <- row.names(Ii.EOO.res)
Ii.EOO.res <- data.frame(Ii.EOO.res, country = Ii.EOO.res.c)
Ii.EOO <- merge(Ii.EOO, Ii.EOO.res, by.x = "country", by.y = "country")

# Hyphantria cunea
Hc <- sppOcc.min %>% filter(scientificName == "Hyphantria cunea")
Hc.EOO <- data.frame(ddlat = Hc$decimalLatitude,
                     ddlon = Hc$decimalLongitude, 
                     country = Hc$country,
                     name = "Hyphantria cunea")
Hc.EOO <- Hc.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Hc.EOO.res <- EOO.computing(Hc.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Hc.EOO.res.c <- row.names(Hc.EOO.res)
Hc.EOO.res <- data.frame(Hc.EOO.res, country = Hc.EOO.res.c)
Hc.EOO <- merge(Hc.EOO, Hc.EOO.res, by.x = "country", by.y = "country")

# Monomorium floricola
Mf <- sppOcc.min %>% filter(scientificName == "Monomorium floricola")
Mf.EOO <- data.frame(ddlat = Mf$decimalLatitude,
                     ddlon = Mf$decimalLongitude, 
                     country = Mf$country,
                     name = "Monomorium floricola")
Mf.EOO <- Mf.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mf.EOO.res <- EOO.computing(Mf.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mf.EOO.res.c <- row.names(Mf.EOO.res)
Mf.EOO.res <- data.frame(Mf.EOO.res, country = Mf.EOO.res.c)
Mf.EOO <- merge(Mf.EOO, Mf.EOO.res, by.x = "country", by.y = "country")

# Paratrechina pubens
Pp <- sppOcc.min %>% filter(scientificName == "Paratrechina pubens")
Pp.EOO <- data.frame(ddlat = Pp$decimalLatitude,
                     ddlon = Pp$decimalLongitude, 
                     country = Pp$country,
                     name = "Paratrechina pubens")
Pp.EOO <- Pp.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pp.EOO.res <- EOO.computing(Pp.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pp.EOO.res.c <- row.names(Pp.EOO.res)
Pp.EOO.res <- data.frame(Pp.EOO.res, country = Pp.EOO.res.c)
Pp.EOO <- merge(Pp.EOO, Pp.EOO.res, by.x = "country", by.y = "country")

# Tetramorium bicarinatum
Tb <- sppOcc.min %>% filter(scientificName == "Tetramorium bicarinatum")
Tb.EOO <- data.frame(ddlat = Tb$decimalLatitude,
                     ddlon = Tb$decimalLongitude, 
                     country = Tb$country,
                     name = "Tetramorium bicarinatum")
Tb.EOO <- Tb.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tb.EOO.res <- EOO.computing(Tb.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tb.EOO.res.c <- row.names(Tb.EOO.res)
Tb.EOO.res <- data.frame(Tb.EOO.res, country = Tb.EOO.res.c)
Tb.EOO <- merge(Tb.EOO, Tb.EOO.res, by.x = "country", by.y = "country")

# Vespa velutina nigrithorax
Vvn <- sppOcc.min %>% filter(scientificName == "Vespa velutina nigrithorax")
Vvn.EOO <- data.frame(ddlat = Vvn$decimalLatitude,
                      ddlon = Vvn$decimalLongitude, 
                      country = Vvn$country,
                      name = "Vespa velutina nigrithorax")
Vvn.EOO <- Vvn.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Vvn.EOO.res <- EOO.computing(Vvn.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Vvn.EOO.res.c <- row.names(Vvn.EOO.res)
Vvn.EOO.res <- data.frame(Vvn.EOO.res, country = Vvn.EOO.res.c)
Vvn.EOO <- merge(Vvn.EOO, Vvn.EOO.res, by.x = "country", by.y = "country")

# Anoplolepsis gracilipes
Ag <- sppOcc.min %>% filter(scientificName == "Anoplolepsis gracilipes")
Ag.EOO <- data.frame(ddlat = Ag$decimalLatitude,
                     ddlon = Ag$decimalLongitude, 
                     country = Ag$country,
                     name = "Anoplolepsis gracilipes")
Ag.EOO <- Ag.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ag.EOO.res <- EOO.computing(Ag.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ag.EOO.res.c <- row.names(Ag.EOO.res)
Ag.EOO.res <- data.frame(Ag.EOO.res, country = Ag.EOO.res.c)
Ag.EOO <- merge(Ag.EOO, Ag.EOO.res, by.x = "country", by.y = "country")

# Pheidole megacephala
Pm <- sppOcc.min %>% filter(scientificName == "Pheidole megacephala")
Pm.EOO <- data.frame(ddlat = Pm$decimalLatitude,
                     ddlon = Pm$decimalLongitude, 
                     country = Pm$country,
                     name = "Pheidole megacephala")
Pm.EOO <- Pm.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pm.EOO.res <- EOO.computing(Pm.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pm.EOO.res.c <- row.names(Pm.EOO.res)
Pm.EOO.res <- data.frame(Pm.EOO.res, country = Pm.EOO.res.c)
Pm.EOO <- merge(Pm.EOO, Pm.EOO.res, by.x = "country", by.y = "country")

# Apis mellifera
Am <- sppOcc.min %>% filter(scientificName == "Apis mellifera")
Am.EOO <- data.frame(ddlat = Am$decimalLatitude,
                     ddlon = Am$decimalLongitude, 
                     country = Am$country,
                     name = "Apis mellifera")
Am.EOO <- Am.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Am.EOO.res <- EOO.computing(Am.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Am.EOO.res.c <- row.names(Am.EOO.res)
Am.EOO.res <- data.frame(Am.EOO.res, country = Am.EOO.res.c)
Am.EOO <- merge(Am.EOO, Am.EOO.res, by.x = "country", by.y = "country")

# Adelges tsugae
Ats <- sppOcc.min %>% filter(scientificName == "Adelges tsugae")
Ats.EOO <- data.frame(ddlat = Ats$decimalLatitude,
                      ddlon = Ats$decimalLongitude, 
                      country = Ats$country,
                      name = "Adelges tsugae")
Ats.EOO <- Ats.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ats.EOO.res <- EOO.computing(Ats.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ats.EOO.res.c <- row.names(Ats.EOO.res)
Ats.EOO.res <- data.frame(Ats.EOO.res, country = Ats.EOO.res.c)
Ats.EOO <- merge(Ats.EOO, Ats.EOO.res, by.x = "country", by.y = "country")

# Xyleborus glabratus
Xg <- sppOcc.min %>% filter(scientificName == "Xyleborus glabratus")
Xg.EOO <- data.frame(ddlat = Xg$decimalLatitude,
                     ddlon = Xg$decimalLongitude, 
                     country = Xg$country,
                     name = "Xyleborus glabratus")
Xg.EOO <- Xg.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Xg.EOO.res <- EOO.computing(Xg.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Xg.EOO.res.c <- row.names(Xg.EOO.res)
Xg.EOO.res <- data.frame(Xg.EOO.res, country = Xg.EOO.res.c)
Xg.EOO <- merge(Xg.EOO, Xg.EOO.res, by.x = "country", by.y = "country")

# Aphis gossypi
Ago <- sppOcc.min %>% filter(scientificName == "Aphis gossypi")
Ago.EOO <- data.frame(ddlat = Ago$decimalLatitude,
                      ddlon = Ago$decimalLongitude, 
                      country = Ago$country,
                      name = "Aphis gossypi")
Ago.EOO <- Ago.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ago.EOO.res <- EOO.computing(Ago.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ago.EOO.res.c <- row.names(Ago.EOO.res)
Ago.EOO.res <- data.frame(Ago.EOO.res, country = Ago.EOO.res.c)
Ago.EOO <- merge(Ago.EOO, Ago.EOO.res, by.x = "country", by.y = "country")

# Bombus terrestris
Bt <- sppOcc.min %>% filter(scientificName == "Bombus terrestris")
Bt.EOO <- data.frame(ddlat = Bt$decimalLatitude,
                     ddlon = Bt$decimalLongitude, 
                     country = Bt$country,
                     name = "Bombus terrestris")
Bt.EOO <- Bt.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bt.EOO.res <- EOO.computing(Bt.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Bt.EOO.res.c <- row.names(Bt.EOO.res)
Bt.EOO.res <- data.frame(Bt.EOO.res, country = Bt.EOO.res.c)
Bt.EOO <- merge(Bt.EOO, Bt.EOO.res, by.x = "country", by.y = "country")

# Paratrechina longicornis
Pl <- sppOcc.min %>% filter(scientificName == "Paratrechina longicornis")
Pl.EOO <- data.frame(ddlat = Pl$decimalLatitude,
                     ddlon = Pl$decimalLongitude, 
                     country = Pl$country,
                     name = "Paratrechina longicornis")
Pl.EOO <- Pl.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pl.EOO.res <- EOO.computing(Pl.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pl.EOO.res.c <- row.names(Pl.EOO.res)
Pl.EOO.res <- data.frame(Pl.EOO.res, country = Pl.EOO.res.c)
Pl.EOO <- merge(Pl.EOO, Pl.EOO.res, by.x = "country", by.y = "country")

# Wasmannia auropunctata
Wa <- sppOcc.min %>% filter(scientificName == "Wasmannia auropunctata")
Wa.EOO <- data.frame(ddlat = Wa$decimalLatitude,
                     ddlon = Wa$decimalLongitude, 
                     country = Wa$country,
                     name = "Wasmannia auropunctata")
Wa.EOO <- Wa.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Wa.EOO.res <- EOO.computing(Wa.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Wa.EOO.res.c <- row.names(Wa.EOO.res)
Wa.EOO.res <- data.frame(Wa.EOO.res, country = Wa.EOO.res.c)
Wa.EOO <- merge(Wa.EOO, Wa.EOO.res, by.x = "country", by.y = "country")

# Linepithema humile
Lh <- sppOcc.min %>% filter(scientificName == "Linepithema humile")
Lh.EOO <- data.frame(ddlat = Lh$decimalLatitude,
                     ddlon = Lh$decimalLongitude, 
                     country = Lh$country,
                     name = "Linepithema humile")
Lh.EOO <- Lh.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Lh.EOO.res <- EOO.computing(Lh.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Lh.EOO.res.c <- row.names(Lh.EOO.res)
Lh.EOO.res <- data.frame(Lh.EOO.res, country = Lh.EOO.res.c)
Lh.EOO <- merge(Lh.EOO, Lh.EOO.res, by.x = "country", by.y = "country")

# Lymantria dispar
Ld <- sppOcc.min %>% filter(scientificName == "Lymantria dispar")
Ld.EOO <- data.frame(ddlat = Ld$decimalLatitude,
                     ddlon = Ld$decimalLongitude, 
                     country = Ld$country,
                     name = "Lymantria dispar")
Ld.EOO <- Ld.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ld.EOO.res <- EOO.computing(Ld.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ld.EOO.res.c <- row.names(Ld.EOO.res)
Ld.EOO.res <- data.frame(Ld.EOO.res, country = Ld.EOO.res.c)
Ld.EOO <- merge(Ld.EOO, Ld.EOO.res, by.x = "country", by.y = "country")

# Lysiphlebus testaceipes
Lt <- sppOcc.min %>% filter(scientificName == "Lysiphlebus testaceipes")
Lt.EOO <- data.frame(ddlat = Lt$decimalLatitude,
                     ddlon = Lt$decimalLongitude, 
                     country = Lt$country,
                     name = "Lysiphlebus testaceipes")
Lt.EOO <- Lt.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Lt.EOO.res <- EOO.computing(Lt.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Lt.EOO.res.c <- row.names(Lt.EOO.res)
Lt.EOO.res <- data.frame(Lt.EOO.res, country = Lt.EOO.res.c)
Lt.EOO <- merge(Lt.EOO, Lt.EOO.res, by.x = "country", by.y = "country")

# Merizodus soledadinus
Ms <- sppOcc.min %>% filter(scientificName == "Merizodus soledadinus")
Ms.EOO <- data.frame(ddlat = Ms$decimalLatitude,
                     ddlon = Ms$decimalLongitude, 
                     country = Ms$country,
                     name = "Merizodus soledadinus")
Ms.EOO <- Ms.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ms.EOO.res <- EOO.computing(Ms.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ms.EOO.res.c <- row.names(Ms.EOO.res)
Ms.EOO.res <- data.frame(Ms.EOO.res, country = Ms.EOO.res.c)
Ms.EOO <- merge(Ms.EOO, Ms.EOO.res, by.x = "country", by.y = "country")

# Myrmica rubra
Mru <- sppOcc.min %>% filter(scientificName == "Myrmica rubra")
Mru.EOO <- data.frame(ddlat = Mru$decimalLatitude,
                      ddlon = Mru$decimalLongitude, 
                      country = Mru$country,
                      name = "Myrmica rubra")
Mru.EOO <- Mru.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mru.EOO.res <- EOO.computing(Mru.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mru.EOO.res.c <- row.names(Mru.EOO.res)
Mru.EOO.res <- data.frame(Mru.EOO.res, country = Mru.EOO.res.c)
Mru.EOO <- merge(Mru.EOO, Mru.EOO.res, by.x = "country", by.y = "country")

# Pachycondyla chinensis
Pc <- sppOcc.min %>% filter(scientificName == "Pachycondyla chinensis")
Pc.EOO <- data.frame(ddlat = Pc$decimalLatitude,
                     ddlon = Pc$decimalLongitude, 
                     country = Pc$country,
                     name = "Pachycondyla chinensis")
Pc.EOO <- Pc.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pc.EOO.res <- EOO.computing(Pc.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pc.EOO.res.c <- row.names(Pc.EOO.res)
Pc.EOO.res <- data.frame(Pc.EOO.res, country = Pc.EOO.res.c)
Pc.EOO <- merge(Pc.EOO, Pc.EOO.res, by.x = "country", by.y = "country")

# Solenopsis invicta
Si <- sppOcc.min %>% filter(scientificName == "Solenopsis invicta")
Si.EOO <- data.frame(ddlat = Si$decimalLatitude,
                     ddlon = Si$decimalLongitude, 
                     country = Si$country,
                     name = "Solenopsis invicta")
Si.EOO <- Si.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Si.EOO.res <- EOO.computing(Si.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Si.EOO.res.c <- row.names(Si.EOO.res)
Si.EOO.res <- data.frame(Si.EOO.res, country = Si.EOO.res.c)
Si.EOO <- merge(Si.EOO, Si.EOO.res, by.x = "country", by.y = "country")

# Vespula vulgaris
Vv <- sppOcc.min %>% filter(scientificName == "Vespula vulgaris")
Vv.EOO <- data.frame(ddlat = Vv$decimalLatitude,
                     ddlon = Vv$decimalLongitude, 
                     country = Vv$country,
                     name = "Vespula vulgaris")
Vv.EOO <- Vv.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Vv.EOO.res <- EOO.computing(Vv.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Vv.EOO.res.c <- row.names(Vv.EOO.res)
Vv.EOO.res <- data.frame(Vv.EOO.res, country = Vv.EOO.res.c)
Vv.EOO <- merge(Vv.EOO, Vv.EOO.res, by.x = "country", by.y = "country")

# Andricus quercuscalicis
Aq <- sppOcc.min %>% filter(scientificName == "Andricus quercuscalicis")
Aq.EOO <- data.frame(ddlat = Aq$decimalLatitude,
                     ddlon = Aq$decimalLongitude, 
                     country = Aq$country,
                     name = "Andricus quercuscalicis")
Aq.EOO <- Aq.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Aq.EOO.res <- EOO.computing(Aq.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Aq.EOO.res.c <- row.names(Aq.EOO.res)
Aq.EOO.res <- data.frame(Aq.EOO.res, country = Aq.EOO.res.c)
Aq.EOO <- merge(Aq.EOO, Aq.EOO.res, by.x = "country", by.y = "country")

# Anoplophora chinensis
Ach <- sppOcc.min %>% filter(scientificName == "Anoplophora chinensis")
Ach.EOO <- data.frame(ddlat = Ach$decimalLatitude,
                      ddlon = Ach$decimalLongitude, 
                      country = Ach$country,
                      name = "Anoplophora chinensis")
Ach.EOO <- Ach.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ach.EOO.res <- EOO.computing(Ach.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ach.EOO.res.c <- row.names(Ach.EOO.res)
Ach.EOO.res <- data.frame(Ach.EOO.res, country = Ach.EOO.res.c)
Ach.EOO <- merge(Ach.EOO, Ach.EOO.res, by.x = "country", by.y = "country")

# Anoplophora glabripennis
Agl <- sppOcc.min %>% filter(scientificName == "Anoplophora glabripennis")
Agl.EOO <- data.frame(ddlat = Agl$decimalLatitude,
                      ddlon = Agl$decimalLongitude, 
                      country = Agl$country,
                      name = "Anoplophora glabripennis")
Agl.EOO <- Agl.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Agl.EOO.res <- EOO.computing(Agl.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Agl.EOO.res.c <- row.names(Agl.EOO.res)
Agl.EOO.res <- data.frame(Agl.EOO.res, country = Agl.EOO.res.c)
Agl.EOO <- merge(Agl.EOO, Agl.EOO.res, by.x = "country", by.y = "country")

# Aulacaspis yasumatsui
Ay <- sppOcc.min %>% filter(scientificName == "Aulacaspis yasumatsui")
Ay.EOO <- data.frame(ddlat = Ay$decimalLatitude,
                     ddlon = Ay$decimalLongitude, 
                     country = Ay$country,
                     name = "Aulacaspis yasumatsui")
Ay.EOO <- Ay.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ay.EOO.res <- EOO.computing(Ay.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ay.EOO.res.c <- row.names(Ay.EOO.res)
Ay.EOO.res <- data.frame(Ay.EOO.res, country = Ay.EOO.res.c)
Ay.EOO <- merge(Ay.EOO, Ay.EOO.res, by.x = "country", by.y = "country")

# Cactoblastis cactorum
Cc <- sppOcc.min %>% filter(scientificName == "Cactoblastis cactorum")
Cc.EOO <- data.frame(ddlat = Cc$decimalLatitude,
                     ddlon = Cc$decimalLongitude, 
                     country = Cc$country,
                     name = "Cactoblastis cactorum")
Cc.EOO <- Cc.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Cc.EOO.res <- EOO.computing(Cc.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Cc.EOO.res.c <- row.names(Cc.EOO.res)
Cc.EOO.res <- data.frame(Cc.EOO.res, country = Cc.EOO.res.c)
Cc.EOO <- merge(Cc.EOO, Cc.EOO.res, by.x = "country", by.y = "country")

# Diaphorina citri
Dc <- sppOcc.min %>% filter(scientificName == "Diaphorina citri")
Dc.EOO <- data.frame(ddlat = Dc$decimalLatitude,
                     ddlon = Dc$decimalLongitude, 
                     country = Dc$country,
                     name = "Diaphorina citri")
Dc.EOO <- Dc.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Dc.EOO.res <- EOO.computing(Dc.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Dc.EOO.res.c <- row.names(Dc.EOO.res)
Dc.EOO.res <- data.frame(Dc.EOO.res, country = Dc.EOO.res.c)
Dc.EOO <- merge(Dc.EOO, Dc.EOO.res, by.x = "country", by.y = "country")

# Homalodisca vitripennis
Hv <- sppOcc.min %>% filter(scientificName == "Homalodisca vitripennis")
Hv.EOO <- data.frame(ddlat = Hv$decimalLatitude,
                     ddlon = Hv$decimalLongitude, 
                     country = Hv$country,
                     name = "Homalodisca vitripennis")
Hv.EOO <- Hv.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Hv.EOO.res <- EOO.computing(Hv.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Hv.EOO.res.c <- row.names(Hv.EOO.res)
Hv.EOO.res <- data.frame(Hv.EOO.res, country = Hv.EOO.res.c)
Hv.EOO <- merge(Hv.EOO, Hv.EOO.res, by.x = "country", by.y = "country")

# Orthotomicus erosus
Oe <- sppOcc.min %>% filter(scientificName == "Orthotomicus erosus")
Oe.EOO <- data.frame(ddlat = Oe$decimalLatitude,
                     ddlon = Oe$decimalLongitude, 
                     country = Oe$country,
                     name = "Orthotomicus erosus")
Oe.EOO <- Oe.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Oe.EOO.res <- EOO.computing(Oe.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Oe.EOO.res.c <- row.names(Oe.EOO.res)
Oe.EOO.res <- data.frame(Oe.EOO.res, country = Oe.EOO.res.c)
Oe.EOO <- merge(Oe.EOO, Oe.EOO.res, by.x = "country", by.y = "country")

# Paratrechina fulva
Pf <- sppOcc.min %>% filter(scientificName == "Paratrechina fulva")
Pf.EOO <- data.frame(ddlat = Pf$decimalLatitude,
                     ddlon = Pf$decimalLongitude, 
                     country = Pf$country,
                     name = "Paratrechina fulva")
Pf.EOO <- Pf.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pf.EOO.res <- EOO.computing(Pf.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pf.EOO.res.c <- row.names(Pf.EOO.res)
Pf.EOO.res <- data.frame(Pf.EOO.res, country = Pf.EOO.res.c)
Pf.EOO <- merge(Pf.EOO, Pf.EOO.res, by.x = "country", by.y = "country")

# Solenopsis richteri
Sr <- sppOcc.min %>% filter(scientificName == "Solenopsis richteri")
Sr.EOO <- data.frame(ddlat = Sr$decimalLatitude,
                     ddlon = Sr$decimalLongitude, 
                     country = Sr$country,
                     name = "Solenopsis richteri")
Sr.EOO <- Sr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Sr.EOO.res <- EOO.computing(Sr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Sr.EOO.res.c <- row.names(Sr.EOO.res)
Sr.EOO.res <- data.frame(Sr.EOO.res, country = Sr.EOO.res.c)
Sr.EOO <- merge(Sr.EOO, Sr.EOO.res, by.x = "country", by.y = "country")

# Technomyrmex albipes
Ta <- sppOcc.min %>% filter(scientificName == "Technomyrmex albipes")
Ta.EOO <- data.frame(ddlat = Ta$decimalLatitude,
                     ddlon = Ta$decimalLongitude, 
                     country = Ta$country,
                     name = "Technomyrmex albipes")
Ta.EOO <- Ta.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ta.EOO.res <- EOO.computing(Ta.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ta.EOO.res.c <- row.names(Ta.EOO.res)
Ta.EOO.res <- data.frame(Ta.EOO.res, country = Ta.EOO.res.c)
Ta.EOO <- merge(Ta.EOO, Ta.EOO.res, by.x = "country", by.y = "country")

# Tetropium fuscum
Tf <- sppOcc.min %>% filter(scientificName == "Tetropium fuscum")
Tf.EOO <- data.frame(ddlat = Tf$decimalLatitude,
                     ddlon = Tf$decimalLongitude, 
                     country = Tf$country,
                     name = "Tetropium fuscum")
Tf.EOO <- Tf.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tf.EOO.res <- EOO.computing(Tf.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tf.EOO.res.c <- row.names(Tf.EOO.res)
Tf.EOO.res <- data.frame(Tf.EOO.res, country = Tf.EOO.res.c)
Tf.EOO <- merge(Tf.EOO, Tf.EOO.res, by.x = "country", by.y = "country")

# Apis cerana
Ace <- sppOcc.min %>% filter(scientificName == "Apis cerana")
Ace.EOO <- data.frame(ddlat = Ace$decimalLatitude,
                      ddlon = Ace$decimalLongitude, 
                      country = Ace$country,
                      name = "Apis cerana")
Ace.EOO <- Ace.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ace.EOO.res <- EOO.computing(Ace.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ace.EOO.res.c <- row.names(Ace.EOO.res)
Ace.EOO.res <- data.frame(Ace.EOO.res, country = Ace.EOO.res.c)
Ace.EOO <- merge(Ace.EOO, Ace.EOO.res, by.x = "country", by.y = "country")

# Quadrastichus erythrinae (No GBIF records)
# Bessa remota (No GBIF records)
# Apis mellifera scutellata (No GBIF records)

# Calliphora vicina
Cv <- sppOcc.min %>% filter(scientificName == "Calliphora vicina")
Cv.EOO <- data.frame(ddlat = Cv$decimalLatitude,
                     ddlon = Cv$decimalLongitude, 
                     country = Cv$country,
                     name = "Calliphora vicina")
Cv.EOO <- Cv.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Cv.EOO.res <- EOO.computing(Cv.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Cv.EOO.res.c <- row.names(Cv.EOO.res)
Cv.EOO.res <- data.frame(Cv.EOO.res, country = Cv.EOO.res.c)
Cv.EOO <- merge(Cv.EOO, Cv.EOO.res, by.x = "country", by.y = "country")

# Cicindelidia trifasciata
Ct <- sppOcc.min %>% filter(scientificName == "Cicindelidia trifasciata")
Ct.EOO <- data.frame(ddlat = Ct$decimalLatitude,
                     ddlon = Ct$decimalLongitude, 
                     country = Ct$country,
                     name = "Cicindelidia trifasciata")
Ct.EOO <- Ct.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ct.EOO.res <- EOO.computing(Ct.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ct.EOO.res.c <- row.names(Ct.EOO.res)
Ct.EOO.res <- data.frame(Ct.EOO.res, country = Ct.EOO.res.c)
Ct.EOO <- merge(Ct.EOO, Ct.EOO.res, by.x = "country", by.y = "country")

# Cinara cupressi
Ccu <- sppOcc.min %>% filter(scientificName == "Cinara cupressi")
Ccu.EOO <- data.frame(ddlat = Ccu$decimalLatitude,
                      ddlon = Ccu$decimalLongitude, 
                      country = Ccu$country,
                      name = "Cinara cupressi")
Ccu.EOO <- Ccu.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ccu.EOO.res <- EOO.computing(Ccu.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ccu.EOO.res.c <- row.names(Ccu.EOO.res)
Ccu.EOO.res <- data.frame(Ccu.EOO.res, country = Ccu.EOO.res.c)
Ccu.EOO <- merge(Ccu.EOO, Ccu.EOO.res, by.x = "country", by.y = "country")

# Danaus plexippus
Dp <- sppOcc.min %>% filter(scientificName == "Danaus plexippus")
Dp.EOO <- data.frame(ddlat = Dp$decimalLatitude,
                     ddlon = Dp$decimalLongitude, 
                     country = Dp$country,
                     name = "Danaus plexippus")
Dp.EOO <- Dp.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Dp.EOO.res <- EOO.computing(Dp.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Dp.EOO.res.c <- row.names(Dp.EOO.res)
Dp.EOO.res <- data.frame(Dp.EOO.res, country = Dp.EOO.res.c)
Dp.EOO <- merge(Dp.EOO, Dp.EOO.res, by.x = "country", by.y = "country")

# Harmonia axyridis
Ha <- sppOcc.min %>% filter(scientificName == "Harmonia axyridis")
Ha.EOO <- data.frame(ddlat = Ha$decimalLatitude,
                     ddlon = Ha$decimalLongitude, 
                     country = Ha$country,
                     name = "Harmonia axyridis")
Ha.EOO <- Ha.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ha.EOO.res <- EOO.computing(Ha.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ha.EOO.res.c <- row.names(Ha.EOO.res)
Ha.EOO.res <- data.frame(Ha.EOO.res, country = Ha.EOO.res.c)
Ha.EOO <- merge(Ha.EOO, Ha.EOO.res, by.x = "country", by.y = "country")

# Icerya purchasi
Ip <- sppOcc.min %>% filter(scientificName == "Icerya purchasi")
Ip.EOO <- data.frame(ddlat = Ip$decimalLatitude,
                     ddlon = Ip$decimalLongitude, 
                     country = Ip$country,
                     name = "Icerya purchasi")
Ip.EOO <- Ip.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ip.EOO.res <- EOO.computing(Ip.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ip.EOO.res.c <- row.names(Ip.EOO.res)
Ip.EOO.res <- data.frame(Ip.EOO.res, country = Ip.EOO.res.c)
Ip.EOO <- merge(Ip.EOO, Ip.EOO.res, by.x = "country", by.y = "country")

# Larinus planus
Lp <- sppOcc.min %>% filter(scientificName == "Larinus planus")
Lp.EOO <- data.frame(ddlat = Lp$decimalLatitude,
                     ddlon = Lp$decimalLongitude, 
                     country = Lp$country,
                     name = "Larinus planus")
Lp.EOO <- Lp.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Lp.EOO.res <- EOO.computing(Lp.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Lp.EOO.res.c <- row.names(Lp.EOO.res)
Lp.EOO.res <- data.frame(Lp.EOO.res, country = Lp.EOO.res.c)
Lp.EOO <- merge(Lp.EOO, Lp.EOO.res, by.x = "country", by.y = "country")

# Lasius neglectus
Ln <- sppOcc.min %>% filter(scientificName == "Lasius neglectus")
Ln.EOO <- data.frame(ddlat = Ln$decimalLatitude,
                     ddlon = Ln$decimalLongitude, 
                     country = Ln$country,
                     name = "Lasius neglectus")
Ln.EOO <- Ln.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ln.EOO.res <- EOO.computing(Ln.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ln.EOO.res.c <- row.names(Ln.EOO.res)
Ln.EOO.res <- data.frame(Ln.EOO.res, country = Ln.EOO.res.c)
Ln.EOO <- merge(Ln.EOO, Ln.EOO.res, by.x = "country", by.y = "country")

# Ochlerotatus japonicus
Oj <- sppOcc.min %>% filter(scientificName == "Ochlerotatus japonicus")
Oj.EOO <- data.frame(ddlat = Oj$decimalLatitude,
                     ddlon = Oj$decimalLongitude, 
                     country = Oj$country,
                     name = "Ochlerotatus japonicus")
Oj.EOO <- Oj.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Oj.EOO.res <- EOO.computing(Oj.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Oj.EOO.res.c <- row.names(Oj.EOO.res)
Oj.EOO.res <- data.frame(Oj.EOO.res, country = Oj.EOO.res.c)
Oj.EOO <- merge(Oj.EOO, Oj.EOO.res, by.x = "country", by.y = "country")

# Pieris rapae
Pr <- sppOcc.min %>% filter(scientificName == "Pieris rapae")
Pr.EOO <- data.frame(ddlat = Pr$decimalLatitude,
                     ddlon = Pr$decimalLongitude, 
                     country = Pr$country,
                     name = "Pieris rapae")
Pr.EOO <- Pr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pr.EOO.res <- EOO.computing(Pr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pr.EOO.res.c <- row.names(Pr.EOO.res)
Pr.EOO.res <- data.frame(Pr.EOO.res, country = Pr.EOO.res.c)
Pr.EOO <- merge(Pr.EOO, Pr.EOO.res, by.x = "country", by.y = "country")

# Pteromalus puparum
Ppu <- sppOcc.min %>% filter(scientificName == "Pteromalus puparum")
Ppu.EOO <- data.frame(ddlat = Ppu$decimalLatitude,
                      ddlon = Ppu$decimalLongitude, 
                      country = Ppu$country,
                      name = "Pteromalus puparum")
Ppu.EOO <- Ppu.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ppu.EOO.res <- EOO.computing(Ppu.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ppu.EOO.res.c <- row.names(Ppu.EOO.res)
Ppu.EOO.res <- data.frame(Ppu.EOO.res, country = Ppu.EOO.res.c)
Ppu.EOO <- merge(Ppu.EOO, Ppu.EOO.res, by.x = "country", by.y = "country")

# Pterostichus melanarius
Pme <- sppOcc.min %>% filter(scientificName == "Pterostichus melanarius")
Pme.EOO <- data.frame(ddlat = Pme$decimalLatitude,
                      ddlon = Pme$decimalLongitude, 
                      country = Pme$country,
                      name = "Pterostichus melanarius")
Pme.EOO <- Pme.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pme.EOO.res <- EOO.computing(Pme.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pme.EOO.res.c <- row.names(Pme.EOO.res)
Pme.EOO.res <- data.frame(Pme.EOO.res, country = Pme.EOO.res.c)
Pme.EOO <- merge(Pme.EOO, Pme.EOO.res, by.x = "country", by.y = "country")

# Solenopsis geminata
Sg <- sppOcc.min %>% filter(scientificName == "Solenopsis geminata")
Sg.EOO <- data.frame(ddlat = Sg$decimalLatitude,
                     ddlon = Sg$decimalLongitude, 
                     country = Sg$country,
                     name = "Solenopsis geminata")
Sg.EOO <- Sg.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Sg.EOO.res <- EOO.computing(Sg.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Sg.EOO.res.c <- row.names(Sg.EOO.res)
Sg.EOO.res <- data.frame(Sg.EOO.res, country = Sg.EOO.res.c)
Sg.EOO <- merge(Sg.EOO, Sg.EOO.res, by.x = "country", by.y = "country")

# Tapinoma melanocephalum
Tm <- sppOcc.min %>% filter(scientificName == "Tapinoma melanocephalum")
Tm.EOO <- data.frame(ddlat = Tm$decimalLatitude,
                     ddlon = Tm$decimalLongitude, 
                     country = Tm$country,
                     name = "Tapinoma melanocephalum")
Tm.EOO <- Tm.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tm.EOO.res <- EOO.computing(Tm.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tm.EOO.res.c <- row.names(Tm.EOO.res)
Tm.EOO.res <- data.frame(Tm.EOO.res, country = Tm.EOO.res.c)
Tm.EOO <- merge(Tm.EOO, Tm.EOO.res, by.x = "country", by.y = "country")

# Torymus sinensis
Tsi <- sppOcc.min %>% filter(scientificName == "Torymus sinensis")
Tsi.EOO <- data.frame(ddlat = Tsi$decimalLatitude,
                      ddlon = Tsi$decimalLongitude, 
                      country = Tsi$country,
                      name = "Torymus sinensis")
Tsi.EOO <- Tsi.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tsi.EOO.res <- EOO.computing(Tsi.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tsi.EOO.res.c <- row.names(Tsi.EOO.res)
Tsi.EOO.res <- data.frame(Tsi.EOO.res, country = Tsi.EOO.res.c)
Tsi.EOO <- merge(Tsi.EOO, Tsi.EOO.res, by.x = "country", by.y = "country")

# Trechisibus antarcticus
Tan <- sppOcc.min %>% filter(scientificName == "Trechisibus antarcticus")
Tan.EOO <- data.frame(ddlat = Tan$decimalLatitude,
                      ddlon = Tan$decimalLongitude, 
                      country = Tan$country,
                      name = "Trechisibus antarcticus")
Tan.EOO <- Tan.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tan.EOO.res <- EOO.computing(Tan.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tan.EOO.res.c <- row.names(Tan.EOO.res)
Tan.EOO.res <- data.frame(Tan.EOO.res, country = Tan.EOO.res.c)
Tan.EOO <- merge(Tan.EOO, Tan.EOO.res, by.x = "country", by.y = "country")

# Trechus obtusus
To <- sppOcc.min %>% filter(scientificName == "Trechus obtusus")
To.EOO <- data.frame(ddlat = To$decimalLatitude,
                     ddlon = To$decimalLongitude, 
                     country = To$country,
                     name = "Trechus obtusus")
To.EOO <- To.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
To.EOO.res <- EOO.computing(To.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
To.EOO.res.c <- row.names(To.EOO.res)
To.EOO.res <- data.frame(To.EOO.res, country = To.EOO.res.c)
To.EOO <- merge(To.EOO, To.EOO.res, by.x = "country", by.y = "country")

# Trichopoda pilipes
Tpi <- sppOcc.min %>% filter(scientificName == "Trichopoda pilipes")
Tpi.EOO <- data.frame(ddlat = Tpi$decimalLatitude,
                      ddlon = Tpi$decimalLongitude, 
                      country = Tpi$country,
                      name = "Trichopoda pilipes")
Tpi.EOO <- Tpi.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tpi.EOO.res <- EOO.computing(Tpi.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tpi.EOO.res.c <- row.names(Tpi.EOO.res)
Tpi.EOO.res <- data.frame(Tpi.EOO.res, country = Tpi.EOO.res.c)
Tpi.EOO <- merge(Tpi.EOO, Tpi.EOO.res, by.x = "country", by.y = "country")

# Urophora affinis
Ua <- sppOcc.min %>% filter(scientificName == "Urophora affinis")
Ua.EOO <- data.frame(ddlat = Ua$decimalLatitude,
                     ddlon = Ua$decimalLongitude, 
                     country = Ua$country,
                     name = "Urophora affinis")
Ua.EOO <- Ua.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Ua.EOO.res <- EOO.computing(Ua.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Ua.EOO.res.c <- row.names(Ua.EOO.res)
Ua.EOO.res <- data.frame(Ua.EOO.res, country = Ua.EOO.res.c)
Ua.EOO <- merge(Ua.EOO, Ua.EOO.res, by.x = "country", by.y = "country")

# Urophora quadrifasciata
Uq <- sppOcc.min %>% filter(scientificName == "Urophora quadrifasciata")
Uq.EOO <- data.frame(ddlat = Uq$decimalLatitude,
                     ddlon = Uq$decimalLongitude, 
                     country = Uq$country,
                     name = "Urophora quadrifasciata")
Uq.EOO <- Uq.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Uq.EOO.res <- EOO.computing(Uq.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Uq.EOO.res.c <- row.names(Uq.EOO.res)
Uq.EOO.res <- data.frame(Uq.EOO.res, country = Uq.EOO.res.c)
Uq.EOO <- merge(Uq.EOO, Uq.EOO.res, by.x = "country", by.y = "country")

# Vespa velutina
Vve <- sppOcc.min %>% filter(scientificName == "Vespa velutina")
Vve.EOO <- data.frame(ddlat = Vve$decimalLatitude,
                      ddlon = Vve$decimalLongitude, 
                      country = Vve$country,
                      name = "Vespa velutina")
Vve.EOO <- Vve.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Vve.EOO.res <- EOO.computing(Vve.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Vve.EOO.res.c <- row.names(Vve.EOO.res)
Vve.EOO.res <- data.frame(Vve.EOO.res, country = Vve.EOO.res.c)
Vve.EOO <- merge(Vve.EOO, Vve.EOO.res, by.x = "country", by.y = "country")

# Vespula pensylvanica
Vp <- sppOcc.min %>% filter(scientificName == "Vespula pensylvanica")
Vp.EOO <- data.frame(ddlat = Vp$decimalLatitude,
                     ddlon = Vp$decimalLongitude, 
                     country = Vp$country,
                     name = "Vespula pensylvanica")
Vp.EOO <- Vp.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Vp.EOO.res <- EOO.computing(Vp.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Vp.EOO.res.c <- row.names(Vp.EOO.res)
Vp.EOO.res <- data.frame(Vp.EOO.res, country = Vp.EOO.res.c)
Vp.EOO <- merge(Vp.EOO, Vp.EOO.res, by.x = "country", by.y = "country")

# Zizeeria labradus
Zl <- sppOcc.min %>% filter(scientificName == "Zizeeria labradus")
Zl.EOO <- data.frame(ddlat = Zl$decimalLatitude,
                     ddlon = Zl$decimalLongitude, 
                     country = Zl$country,
                     name = "Zizeeria labradus")
Zl.EOO <- Zl.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Zl.EOO.res <- EOO.computing(Zl.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Zl.EOO.res.c <- row.names(Zl.EOO.res)
Zl.EOO.res <- data.frame(Zl.EOO.res, country = Zl.EOO.res.c)
Zl.EOO <- merge(Zl.EOO, Zl.EOO.res, by.x = "country", by.y = "country")

# Bactrocera tryoni
Btr <- sppOcc.min %>% filter(scientificName == "Bactrocera tryoni")
Btr.EOO <- data.frame(ddlat = Btr$decimalLatitude,
                      ddlon = Btr$decimalLongitude, 
                      country = Btr$country,
                      name = "Bactrocera tryoni")
Btr.EOO <- Btr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Btr.EOO.res <- EOO.computing(Btr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Btr.EOO.res.c <- row.names(Btr.EOO.res)
Btr.EOO.res <- data.frame(Btr.EOO.res, country = Btr.EOO.res.c)
Btr.EOO <- merge(Btr.EOO, Btr.EOO.res, by.x = "country", by.y = "country")

# Chilo partellus
Cpa <- sppOcc.min %>% filter(scientificName == "Chilo partellus")
Cpa.EOO <- data.frame(ddlat = Cpa$decimalLatitude,
                      ddlon = Cpa$decimalLongitude, 
                      country = Cpa$country,
                      name = "Chilo partellus")
Cpa.EOO <- Cpa.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Cpa.EOO.res <- EOO.computing(Cpa.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Cpa.EOO.res.c <- row.names(Cpa.EOO.res)
Cpa.EOO.res <- data.frame(Cpa.EOO.res, country = Cpa.EOO.res.c)
Cpa.EOO <- merge(Cpa.EOO, Cpa.EOO.res, by.x = "country", by.y = "country")

# Polistes dominula
Pd <- sppOcc.min %>% filter(scientificName == "Polistes dominula")
Pd.EOO <- data.frame(ddlat = Pd$decimalLatitude,
                     ddlon = Pd$decimalLongitude, 
                     country = Pd$country,
                     name = "Polistes dominula")
Pd.EOO <- Pd.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pd.EOO.res <- EOO.computing(Pd.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pd.EOO.res.c <- row.names(Pd.EOO.res)
Pd.EOO.res <- data.frame(Pd.EOO.res, country = Pd.EOO.res.c)
Pd.EOO <- merge(Pd.EOO, Pd.EOO.res, by.x = "country", by.y = "country")

# Scyphophorus acupunctatus
Sa <- sppOcc.min %>% filter(scientificName == "Scyphophorus acupunctatus")
Sa.EOO <- data.frame(ddlat = Sa$decimalLatitude,
                     ddlon = Sa$decimalLongitude, 
                     country = Sa$country,
                     name = "Scyphophorus acupunctatus")
Sa.EOO <- Sa.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Sa.EOO.res <- EOO.computing(Sa.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Sa.EOO.res.c <- row.names(Sa.EOO.res)
Sa.EOO.res <- data.frame(Sa.EOO.res, country = Sa.EOO.res.c)
Sa.EOO <- merge(Sa.EOO, Sa.EOO.res, by.x = "country", by.y = "country")

# Thaumetopoea processionea
Tpr <- sppOcc.min %>% filter(scientificName == "Thaumetopoea processionea")
Tpr.EOO <- data.frame(ddlat = Tpr$decimalLatitude,
                      ddlon = Tpr$decimalLongitude, 
                      country = Tpr$country,
                      name = "Thaumetopoea processionea")
Tpr.EOO <- Tpr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tpr.EOO.res <- EOO.computing(Tpr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tpr.EOO.res.c <- row.names(Tpr.EOO.res)
Tpr.EOO.res <- data.frame(Tpr.EOO.res, country = Tpr.EOO.res.c)
Tpr.EOO <- merge(Tpr.EOO, Tpr.EOO.res, by.x = "country", by.y = "country")

# Trissolcus basalis
Tba <- sppOcc.min %>% filter(scientificName == "Trissolcus basalis")
Tba.EOO <- data.frame(ddlat = Tba$decimalLatitude,
                      ddlon = Tba$decimalLongitude, 
                      country = Tba$country,
                      name = "Trissolcus basalis")
Tba.EOO <- Tba.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tba.EOO.res <- EOO.computing(Tba.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tba.EOO.res.c <- row.names(Tba.EOO.res)
Tba.EOO.res <- data.frame(Tba.EOO.res, country = Tb.EOO.res.c)
Tba.EOO <- merge(Tba.EOO, Tba.EOO.res, by.x = "country", by.y = "country")

# Vespula germanica 
Vg <- sppOcc.min %>% filter(scientificName == "Vespula germanica")
Vg.EOO <- data.frame(ddlat = Vg$decimalLatitude,
                     ddlon = Vg$decimalLongitude, 
                     country = Vg$country,
                     name = "Vespula germanica")
Vg.EOO <- Vg.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                            ddlon = as.numeric(as.character.numeric_version(ddlon)))
Vg.EOO.res <- EOO.computing(Vg.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Vg.EOO.res.c <- row.names(Vg.EOO.res)
Vg.EOO.res <- data.frame(Vg.EOO.res, country = Vg.EOO.res.c)
Vg.EOO <- merge(Vg.EOO, Vg.EOO.res, by.x = "country", by.y = "country")

# Aedes albopictus
Aal <- sppOcc.min %>% filter(scientificName == "Aedes albopictus")
Aal.EOO <- data.frame(ddlat = Aal$decimalLatitude,
                      ddlon = Aal$decimalLongitude, 
                      country = Aal$country,
                      name = "Aedes albopictus")
Aal.EOO <- Aal.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Aal.EOO.res <- EOO.computing(Aal.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Aal.EOO.res.c <- row.names(Aal.EOO.res)
Aal.EOO.res <- data.frame(Aal.EOO.res, country = Aal.EOO.res.c)
Aal.EOO <- merge(Aal.EOO, Aal.EOO.res, by.x = "country", by.y = "country")

# Anopheles quadrimaculatus
Aqu <- sppOcc.min %>% filter(scientificName == "Anopheles quadrimaculatus")
Aqu.EOO <- data.frame(ddlat = Aqu$decimalLatitude,
                      ddlon = Aqu$decimalLongitude, 
                      country = Aqu$country,
                      name = "Anopheles quadrimaculatus")
Aqu.EOO <- Aqu.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Aqu.EOO.res <- EOO.computing(Aqu.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Aqu.EOO.res.c <- row.names(Aqu.EOO.res)
Aqu.EOO.res <- data.frame(Aqu.EOO.res, country = Aqu.EOO.res.c)
Aqu.EOO <- merge(Aqu.EOO, Aqu.EOO.res, by.x = "country", by.y = "country")

# Microctonus aethiopoides
Mae <- sppOcc.min %>% filter(scientificName == "Microctonus aethiopoides")
Mae.EOO <- data.frame(ddlat = Mae$decimalLatitude,
                      ddlon = Mae$decimalLongitude, 
                      country = Mae$country,
                      name = "Microctonus aethiopoides")
Mae.EOO <- Mae.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mae.EOO.res <- EOO.computing(Mae.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mae.EOO.res.c <- row.names(Mae.EOO.res)
Mae.EOO.res <- data.frame(Mae.EOO.res, country = Mae.EOO.res.c)
Mae.EOO <- merge(Mae.EOO, Mae.EOO.res, by.x = "country", by.y = "country")

# Monomorium destructor
Mde <- sppOcc.min %>% filter(scientificName == "Monomorium destructor")
Mde.EOO <- data.frame(ddlat = Mde$decimalLatitude,
                      ddlon = Mde$decimalLongitude, 
                      country = Mde$country,
                      name = "Monomorium destructor")
Mde.EOO <- Mde.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mde.EOO.res <- EOO.computing(Mde.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mde.EOO.res.c <- row.names(Mde.EOO.res)
Mde.EOO.res <- data.frame(Mde.EOO.res, country = Mde.EOO.res.c)
Mde.EOO <- merge(Mde.EOO, Mde.EOO.res, by.x = "country", by.y = "country")

# Monomorium pharaonis
Mph <- sppOcc.min %>% filter(scientificName == "Monomorium pharaonis")
Mph.EOO <- data.frame(ddlat = Mph$decimalLatitude,
                      ddlon = Mph$decimalLongitude, 
                      country = Mph$country,
                      name = "Monomorium pharaonis")
Mph.EOO <- Mph.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mph.EOO.res <- EOO.computing(Mph.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mph.EOO.res.c <- row.names(Mph.EOO.res)
Mph.EOO.res <- data.frame(Mph.EOO.res, country = Mph.EOO.res.c)
Mph.EOO <- merge(Mph.EOO, Mph.EOO.res, by.x = "country", by.y = "country")

# Myzus ascalonicus
Mas <- sppOcc.min %>% filter(scientificName == "Myzus ascalonicus")
Mas.EOO <- data.frame(ddlat = Mas$decimalLatitude,
                      ddlon = Mas$decimalLongitude, 
                      country = Mas$country,
                      name = "Myzus ascalonicus")
Mas.EOO <- Mas.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mas.EOO.res <- EOO.computing(Mas.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mas.EOO.res.c <- row.names(Mas.EOO.res)
Mas.EOO.res <- data.frame(Mas.EOO.res, country = Mas.EOO.res.c)
Mas.EOO <- merge(Mas.EOO, Mas.EOO.res, by.x = "country", by.y = "country")

# Solenopsis papuana
Spa <- sppOcc.min %>% filter(scientificName == "Solenopsis papuana")
Spa.EOO <- data.frame(ddlat = Spa$decimalLatitude,
                      ddlon = Spa$decimalLongitude, 
                      country = Spa$country,
                      name = "Solenopsis papuana")
Spa.EOO <- Spa.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Spa.EOO.res <- EOO.computing(Spa.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Spa.EOO.res.c <- row.names(Spa.EOO.res)
Spa.EOO.res <- data.frame(Spa.EOO.res, country = Spa.EOO.res.c)
Spa.EOO <- merge(Spa.EOO, Spa.EOO.res, by.x = "country", by.y = "country")

# Tuta absoluta
Tab <- sppOcc.min %>% filter(scientificName == "Tuta absoluta")
Tab.EOO <- data.frame(ddlat = Tab$decimalLatitude,
                      ddlon = Tab$decimalLongitude, 
                      country = Tab$country,
                      name = "Tuta absoluta")
Tab.EOO <- Tab.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tab.EOO.res <- EOO.computing(Tab.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tab.EOO.res.c <- row.names(Tab.EOO.res)
Tab.EOO.res <- data.frame(Tab.EOO.res, country = Tab.EOO.res.c)
Tab.EOO <- merge(Tab.EOO, Tab.EOO.res, by.x = "country", by.y = "country")

# Xylosandrus compactus
Xco <- sppOcc.min %>% filter(scientificName == "Xylosandrus compactus")
Xco.EOO <- data.frame(ddlat = Xco$decimalLatitude,
                      ddlon = Xco$decimalLongitude, 
                      country = Xco$country,
                      name = "Xylosandrus compactus")
Xco.EOO <- Xco.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Xco.EOO.res <- EOO.computing(Xco.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Xco.EOO.res.c <- row.names(Xco.EOO.res)
Xco.EOO.res <- data.frame(Xco.EOO.res, country = Xco.EOO.res.c)
Xco.EOO <- merge(Xco.EOO, Xco.EOO.res, by.x = "country", by.y = "country")

# Chaetosiphon fragaefolii
Cfr <- sppOcc.min %>% filter(scientificName == "Chaetosiphon fragaefolii")
Cfr.EOO <- data.frame(ddlat = Cfr$decimalLatitude,
                      ddlon = Cfr$decimalLongitude, 
                      country = Cfr$country,
                      name = "Chaetosiphon fragaefolii")
Cfr.EOO <- Cfr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Cfr.EOO.res <- EOO.computing(Cfr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Cfr.EOO.res.c <- row.names(Cfr.EOO.res)
Cfr.EOO.res <- data.frame(Cfr.EOO.res, country = Cfr.EOO.res.c)
Cfr.EOO <- merge(Cfr.EOO, Cfr.EOO.res, by.x = "country", by.y = "country")

# Polistes chinensis
Pch <- sppOcc.min %>% filter(scientificName == "Polistes chinensis")
Pch.EOO <- data.frame(ddlat = Pch$decimalLatitude,
                      ddlon = Pch$decimalLongitude, 
                      country = Pch$country,
                      name = "Polistes chinensis")
Pch.EOO <- Pch.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Pch.EOO.res <- EOO.computing(Pch.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Pch.EOO.res.c <- row.names(Pch.EOO.res)
Pch.EOO.res <- data.frame(Pch.EOO.res, country = Pch.EOO.res.c)
Pch.EOO <- merge(Pch.EOO, Pch.EOO.res, by.x = "country", by.y = "country")

# Rhyzopertha dominica
Rdo <- sppOcc.min %>% filter(scientificName == "Rhyzopertha dominica")
Rdo.EOO <- data.frame(ddlat = Rdo$decimalLatitude,
                      ddlon = Rdo$decimalLongitude, 
                      country = Rdo$country,
                      name = "Rhyzopertha dominica")
Rdo.EOO <- Rdo.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Rdo.EOO.res <- EOO.computing(Rdo.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Rdo.EOO.res.c <- row.names(Rdo.EOO.res)
Rdo.EOO.res <- data.frame(Rdo.EOO.res, country = Rdo.EOO.res.c)
Rdo.EOO <- merge(Rdo.EOO, Rdo.EOO.res, by.x = "country", by.y = "country")

# Acanthoscelides obtectus
Aob <- sppOcc.min %>% filter(scientificName == "Acanthoscelides obtectus")
Aob.EOO <- data.frame(ddlat = Aob$decimalLatitude,
                      ddlon = Aob$decimalLongitude, 
                      country = Aob$country,
                      name = "Acanthoscelides obtectus")
Aob.EOO <- Aob.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Aob.EOO.res <- EOO.computing(Aob.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Aob.EOO.res.c <- row.names(Aob.EOO.res)
Aob.EOO.res <- data.frame(Aob.EOO.res, country = Aob.EOO.res.c)
Aob.EOO <- merge(Aob.EOO, Aob.EOO.res, by.x = "country", by.y = "country")

# Bruchus rufimanus
Bru <- sppOcc.min %>% filter(scientificName == "Bruchus rufimanus")
Bru.EOO <- data.frame(ddlat = Bru$decimalLatitude,
                      ddlon = Bru$decimalLongitude, 
                      country = Bru$country,
                      name = "Bruchus rufimanus")
Bru.EOO <- Bru.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bru.EOO.res <- EOO.computing(Bru.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Bru.EOO.res.c <- row.names(Bru.EOO.res)
Bru.EOO.res <- data.frame(Bru.EOO.res, country = Bru.EOO.res.c)
Bru.EOO <- merge(Bru.EOO, Bru.EOO.res, by.x = "country", by.y = "country")

# Drosophila subobscura
Dsu <- sppOcc.min %>% filter(scientificName == "Drosophila subobscura")
Dsu.EOO <- data.frame(ddlat = Dsu$decimalLatitude,
                      ddlon = Dsu$decimalLongitude, 
                      country = Dsu$country,
                      name = "Drosophila subobscura")
Dsu.EOO <- Dsu.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Dsu.EOO.res <- EOO.computing(Dsu.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Dsu.EOO.res.c <- row.names(Dsu.EOO.res)
Dsu.EOO.res <- data.frame(Dsu.EOO.res, country = Dsu.EOO.res.c)
Dsu.EOO <- merge(Dsu.EOO, Dsu.EOO.res, by.x = "country", by.y = "country")

# Rhopalosiphum maidis
Rma <- sppOcc.min %>% filter(scientificName == "Rhopalosiphum maidis")
Rma.EOO <- data.frame(ddlat = Rma$decimalLatitude,
                      ddlon = Rma$decimalLongitude, 
                      country = Rma$country,
                      name = "Rhopalosiphum maidis")
Rma.EOO <- Rma.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Rma.EOO.res <- EOO.computing(Rma.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Rma.EOO.res.c <- row.names(Rma.EOO.res)
Rma.EOO.res <- data.frame(Rma.EOO.res, country = Rma.EOO.res.c)
Rma.EOO <- merge(Rma.EOO, Rma.EOO.res, by.x = "country", by.y = "country")

# Sitophilus oryzae
Sor <- sppOcc.min %>% filter(scientificName == "Sitophilus oryzae")
Sor.EOO <- data.frame(ddlat = Sor$decimalLatitude,
                      ddlon = Sor$decimalLongitude, 
                      country = Sor$country,
                      name = "Sitophilus oryzae")
Sor.EOO <- Sor.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Sor.EOO.res <- EOO.computing(Sor.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Sor.EOO.res.c <- row.names(Sor.EOO.res)
Sor.EOO.res <- data.frame(Sor.EOO.res, country = Sor.EOO.res.c)
Sor.EOO <- merge(Sor.EOO, Sor.EOO.res, by.x = "country", by.y = "country")

# Sitotroga cerealella
Sce <- sppOcc.min %>% filter(scientificName == "Sitotroga cerealella")
Sce.EOO <- data.frame(ddlat = Sce$decimalLatitude,
                      ddlon = Sce$decimalLongitude, 
                      country = Sce$country,
                      name = "Sitotroga cerealella")
Sce.EOO <- Sce.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Sce.EOO.res <- EOO.computing(Sce.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Sce.EOO.res.c <- row.names(Sce.EOO.res)
Sce.EOO.res <- data.frame(Sce.EOO.res, country = Sce.EOO.res.c)
Sce.EOO <- merge(Sce.EOO, Sce.EOO.res, by.x = "country", by.y = "country")

# Bruchus pisorum
Bpi <- sppOcc.min %>% filter(scientificName == "Bruchus pisorum")
Bpi.EOO <- data.frame(ddlat = Bpi$decimalLatitude,
                      ddlon = Bpi$decimalLongitude, 
                      country = Bpi$country,
                      name = "Bruchus pisorum")
Bpi.EOO <- Bpi.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Bpi.EOO.res <- EOO.computing(Bpi.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Bpi.EOO.res.c <- row.names(Bpi.EOO.res)
Bpi.EOO.res <- data.frame(Bpi.EOO.res, country = Bpi.EOO.res.c)
Bpi.EOO <- merge(Bpi.EOO, Bpi.EOO.res, by.x = "country", by.y = "country")

# Leptinotarsa decemlineata
Lde <- sppOcc.min %>% filter(scientificName == "Leptinotarsa decemlineata")
Lde.EOO <- data.frame(ddlat = Lde$decimalLatitude,
                      ddlon = Lde$decimalLongitude, 
                      country = Lde$country,
                      name = "Leptinotarsa decemlineata")
Lde.EOO <- Lde.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Lde.EOO.res <- EOO.computing(Lde.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Lde.EOO.res.c <- row.names(Lde.EOO.res)
Lde.EOO.res <- data.frame(Lde.EOO.res, country = Lde.EOO.res.c)
Lde.EOO <- merge(Lde.EOO, Lde.EOO.res, by.x = "country", by.y = "country")

# Liriomyza huidobrensis
Lhu <- sppOcc.min %>% filter(scientificName == "Liriomyza huidobrensis")
Lhu.EOO <- data.frame(ddlat = Lhu$decimalLatitude,
                      ddlon = Lhu$decimalLongitude, 
                      country = Lhu$country,
                      name = "Liriomyza huidobrensis")
Lhu.EOO <- Lhu.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Lhu.EOO.res <- EOO.computing(Lhu.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Lhu.EOO.res.c <- row.names(Lhu.EOO.res)
Lhu.EOO.res <- data.frame(Lhu.EOO.res, country = Lhu.EOO.res.c)
Lhu.EOO <- merge(Lhu.EOO, Lhu.EOO.res, by.x = "country", by.y = "country")

# Megachile rotundata
Mro <- sppOcc.min %>% filter(scientificName == "Megachile rotundata")
Mro.EOO <- data.frame(ddlat = Mro$decimalLatitude,
                      ddlon = Mro$decimalLongitude, 
                      country = Mro$country,
                      name = "Megachile rotundata")
Mro.EOO <- Mro.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Mro.EOO.res <- EOO.computing(Mro.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Mro.EOO.res.c <- row.names(Mro.EOO.res)
Mro.EOO.res <- data.frame(Mro.EOO.res, country = Mro.EOO.res.c)
Mro.EOO <- merge(Mro.EOO, Mro.EOO.res, by.x = "country", by.y = "country")

# Trogoderma granarium 
Tgr <- sppOcc.min %>% filter(scientificName == "Trogoderma granarium")
Tgr.EOO <- data.frame(ddlat = Tgr$decimalLatitude,
                      ddlon = Tgr$decimalLongitude, 
                      country = Tgr$country,
                      name = "Trogoderma granarium")
Tgr.EOO <- Tgr.EOO %>% mutate(ddlat = as.numeric(as.character.numeric_version(ddlat)),
                              ddlon = as.numeric(as.character.numeric_version(ddlon)))
Tgr.EOO.res <- EOO.computing(Tgr.EOO, method.range = "alpha.hull",method.less.than3 = "arbitrary")
Tgr.EOO.res.c <- row.names(Tgr.EOO.res)
Tgr.EOO.res <- data.frame(Tgr.EOO.res, country = Tgr.EOO.res.c)
Tgr.EOO <- merge(Tgr.EOO, Tgr.EOO.res, by.x = "country", by.y = "country")

EOO.Sum <- bind_rows(Ts.EOO,Cn.EOO,Amc.EOO,Bh.EOO,Bi.EOO,Bl.EOO,Bp.EOO,Br.EOO,Al.EOO,
                     Dk.EOO,Ds.EOO,Aa.EOO,At.EOO,Mr.EOO,Ae.EOO,Ac.EOO,Cg.EOO,Fa.EOO,Ls.EOO,
                     Ar.EOO,Pam.EOO,Pau.EOO,Rf.EOO,Ti.EOO,Rc.EOO,Or.EOO,Un.EOO,Tp.EOO,
                     Ii.EOO,Hc.EOO,Mf.EOO,Pp.EOO,Tb.EOO,Vvn.EOO,Ag.EOO,Pm.EOO,Am.EOO,Ats.EOO,
                     Xg.EOO,Ago.EOO,Bt.EOO,Pl.EOO,Wa.EOO,Lh.EOO,Ld.EOO,Lt.EOO,Ms.EOO,Mru.EOO,
                     Pc.EOO,Si.EOO,Vv.EOO,Aq.EOO,Ach.EOO,Agl.EOO,Ay.EOO,Cc.EOO,Dc.EOO,Hv.EOO,
                     Oe.EOO,Pf.EOO,Sr.EOO,Ta.EOO,Tf.EOO,Ace.EOO,Cv.EOO,Ct.EOO,Ccu.EOO,Dp.EOO,
                     Ha.EOO,Ip.EOO,Lp.EOO,Ln.EOO,Oj.EOO,Pr.EOO,Ppu.EOO,Pme.EOO,Sg.EOO,Tm.EOO,
                     Tsi.EOO,Tan.EOO,To.EOO,Tpi.EOO,Ua.EOO,Uq.EOO,Vve.EOO,Vp.EOO,Zl.EOO,Btr.EOO,
                     Cpa.EOO,Pd.EOO,Sa.EOO,Tpr.EOO,Tba.EOO,Vg.EOO,Aal.EOO,Aqu.EOO,Mae.EOO,Mde.EOO,
                     Mph.EOO,Mas.EOO,Spa.EOO,Tab.EOO,Xco.EOO,Cfr.EOO,Pch.EOO,Rdo.EOO,Aob.EOO,
                     Bru.EOO,Dsu.EOO,Rma.EOO,Sor.EOO,Sce.EOO,Bpi.EOO,Lde.EOO,Lhu.EOO,Mro.EOO,
                     Tgr.EOO)
