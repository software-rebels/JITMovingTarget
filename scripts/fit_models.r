source("scripts/experimental_settings.r")

fit.models <- function(jitdata) {
  fitter <- get(FITTOOL)

  formfunc <- get(paste(PROJECT_NAME, "fit.formulas", STRATA_PER_YEAR, sep="."))
  forms <- formfunc()

  fits <- list()
  for (i in 1:length(forms)) {
    mystrata = i - 1
    local = subset(jitdata, strata == mystrata)
    all = subset(jitdata, strata %in% 0:mystrata)

    fits[[i]] <- list(
                  data = local,
                  train = all,
                  fit.local = do.call(fitter,
                                      append(
                                             list(
                                                  formula = forms[[i]]$local,
                                                  data = local
                                             ),
                                             FITTOOL_PARMS
                                      )
                              ),
                  fit.all = do.call(fitter,
                                    append(
                                           list(
                                                formula = forms[[i]]$all,
                                                data = all
                                           ),
                                           FITTOOL_PARMS
                                    )
                            )
             )
  }

  return(fits)
}

qt.fit.formulas.2 <- function() {
  return(list(
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + rsawr + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + asawr + rsawr + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
)
  ))
}

qt.fit.formulas.4 <- function() {
  return(list(
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + rsawr + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + asawr + rsawr + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
),
list(
    local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix,
    all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns + SPL(nrev,3) +
        SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) + SPL(nuc,3) +
        app + SPL(aexp,3) + SPL(rexp,3) + SPL(asawr,3) + SPL(rsawr,3) + fix
)
              )
  )
}

openstack.fit.formulas.2 <- function() {
  return(list(
              #1
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + app + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + app + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #2
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #3
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #4
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #5
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              )
              ))
}

openstack.fit.formulas.4 <- function() {
  return(list(
              #1
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + app + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + app + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #2
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #3
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #4
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #5
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #6
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #7
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(rtime,3) + SPL(hcmt,3) + self + SPL(age,3) +
                      SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) + SPL(rexp,3) +
                      SPL(rsawr,3)
              ),
              #8
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(ent,3) + SPL(rtime,3) + SPL(hcmt,3) + self +
                      SPL(age,3) + SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) +
                      SPL(rexp,3) + SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(ent,3) + SPL(rtime,3) + SPL(hcmt,3) + self +
                      SPL(age,3) + SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) +
                      SPL(rexp,3) + SPL(rsawr,3)
              ),
              #9
              list(
                   local = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(ent,3) + SPL(rtime,3) + SPL(hcmt,3) + self +
                      SPL(age,3) + SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) +
                      SPL(rexp,3) + SPL(rsawr,3),
                   all = buggy ~ SPL(la,3) + SPL(ld,3) + SPL(nf,3) + ns +
                      SPL(ent,3) + SPL(rtime,3) + SPL(hcmt,3) + self +
                      SPL(age,3) + SPL(nuc,3) + SPL(app,3) + SPL(aexp,3) +
                      SPL(rexp,3) + SPL(rsawr,3)
              )
        )
  )
}
