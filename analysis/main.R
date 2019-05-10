# Random community with approximately appropriate number of species,
# number of individuals,
# and mean max/min body sizes.

sim_community <- simulate_community(nspecies = 10,
                                    nind = 200,
                                    min_mass = 5,
                                    max_mass = 150)

sim_table = make_community_table(sim_community)

bodysize_ed <- make_bsed(sim_table)

dom <- energetic_dominance(sim_table)
