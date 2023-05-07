using(DataFrames)
using(CSV)
using(RCall)
using(LinearAlgebra)
using(Distributions)
using(Distributed)
using(UnicodePlots)

haywardfulldata = CSV.read("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/data_hayward_all.csv",header=true,DataFrame);

delongdata = CSV.read("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/data_delong_mammal.csv",header=true,DataFrame);

# haypredmass = haywardfulldata[!,:Predbodymaskg];
# haypreymass = haywardfulldata[!,:Preybodymasskg];
# haypercent = haywardfulldata[!,:PercentOfKills];

# "Panthera leo"
# "Crocuta crocuta"
# "Panthera pardus"
# "Cuon alpinus"
# "Lycaon pictus"
# "Acinonyx jubatus"
# "Panthera tigris"

# groupsize = [4.1, 2.36, 1, 1, 4, 1, 1]

# Cut out predators < 50 kg
# unique(haywardfulldata[!,:Predbodymasskg])
smallpreds = findall(x->x<50,haywardfulldata[!,:Predbodymasskg]);
haywardfulldata = delete!(haywardfulldata,smallpreds);
#NOTE: if you do this, append _large to output!

#Caculcate mean preferred mass per predator
preds = unique(haywardfulldata[!,:Predator]);
# preds = preds[[1,2,3,4,6]]
#SIMULATION FIT
function genpredprey()
    preyinds = Array{Float64}(undef,0);
    predinds = Array{Float64}(undef,0);
    meancarnivore = Array{Float64}(undef,0);
    meanherbivore = Array{Float64}(undef,0);
    meancarnivorediet = Array{Float64}(undef,length(preds));
    for i=1:length(preds)
        pref = haywardfulldata[!,:PercentOfKills][findall(x->x==preds[i],haywardfulldata[!,:Predator])];
        pref[findall(isnan,pref)].=0.;
        #Average non-zero entries
        # nonzeromean = mean(pref[findall(!iszero,pref)]);
        # pref[findall(iszero,pref)].=nonzeromean;
        # pref = round.((pref)*10000);
        pref = round.((pref ./ sum(pref))*1000); #10000
        preyind = sum(pref);

        meanpredmass = mean(haywardfulldata[!,:Predbodymasskg][findall(x->x==preds[i],haywardfulldata[!,:Predator])]);

        #Save mean predator value
        push!(meancarnivore,meanpredmass);

        # meanpredmass *= groupsize[i];
        predmassSD = 0.1*meanpredmass;
        predbodysizedist = Normal(meanpredmass,predmassSD);

        #DRAW BODY SIZES FOR PREDATOR INDIVIDUALS
        predinds_draw = abs.(rand(predbodysizedist,Int64(preyind)));
        predinds = [predinds; predinds_draw];
        preyi = haywardfulldata[!,:Prey][findall(x->x==preds[i],haywardfulldata[!,:Predator])];
        
        #Make empty preyinds list for predator i
        preyinds_predi = Array{Float64}(undef,0);
        for j=1:length(preyi)
            if pref[j] > 0.
                #draw body masses
                #Preybodymasskg
                #Preybodymasskg34adultfemalemass
                meanmass = haywardfulldata[!,:Preybodymasskg34adultfemalemass][findall(x->x==preds[i],haywardfulldata[!,:Predator])][j];
                #Save mean herbivore value
                push!(meanherbivore,meanmass);
                massSD = 0.25*meanmass;
                preybodysizedist = Normal(meanmass,massSD);
                numbers = Int64(pref[j]);
                preyinds_draw = abs.(rand(preybodysizedist,numbers));
                #Build predator-specific prey inds first, then contatenate to full list after loop
                #NOTE: checked and this doesn't change anything
                preyinds_predi = [preyinds_predi; preyinds_draw];
            end
        end
        meancarnivorediet[i] = mean(preyinds_predi);
        #Concatenate full list
        preyinds = [preyinds; preyinds_predi];
        
    end
    return(predinds,preyinds,meancarnivore,meanherbivore,meancarnivorediet)
end

#General sampling function
function samplinginteraction(xdata,ydata,s,xmin)
    xmax = log10(maximum(xdata));
    # xmin = log10(minimum(xdata));
    xmin = log10(xmin);
    stepsize = (xmax - xmin)/s;
    xexpbins = collect(xmin:stepsize:xmax);
    meany = Array{Float64}(undef,(length(xexpbins)-1));
    meanx = Array{Float64}(undef,(length(xexpbins)-1));
    for i=2:length(xexpbins)
        xdata_pos = findall(x->((x>10^xexpbins[i-1]) && (x < 10^xexpbins[i])), xdata);
        if length(xdata_pos) > 0
            meany[i-1] = mean(ydata[xdata_pos]);
            meanx[i-1] = mean([10^xexpbins[i-1],10^xexpbins[i]]);
        else 
            meany[i-1] =NaN;
            meanx[i-1] =NaN;
        end
    end
    filledspots = findall(!isnan,meany);
    return meanx[filledspots], meany[filledspots] #(10 .^predsizeclass[filledspots])
end


function lineartablebuild(x,y)
    #Expected Prey size as a function of predator size
    R"""
    linearmodel = lm(log($y) ~ log($x))
    summary(linearmodel)
    fitintercept = linearmodel[[1]][[1]];
    fitslope = linearmodel[[1]][[2]];
    CI = confint(linearmodel,level=0.95)
    intlow = CI[[1]];
    slopelow = CI[[2]];
    inthigh = CI[[3]];
    slopehigh = CI[[4]];
    """
    fitintercept = @rget fitintercept;
    fitslope = @rget fitslope;
    intlow = @rget intlow;
    slopelow = @rget slopelow;
    inthigh = @rget inthigh;
    slopehigh = @rget slopehigh;

    #Export for the mathematica notebook
    fit_table = DataFrame([fitintercept intlow inthigh; fitslope slopelow slopehigh], :auto);
    rename!(fit_table,[:Fit,:FitLow,:FitHigh])
    return fit_table
end


predinds,preyinds,meancarnivore,meanherbivore,meancarnivorediet = genpredprey();
R"""
par(mfrow=c(2,2))
plot($predinds,$preyinds,log='xy')
"""
sampledpredinds, sampledpreyinds = samplinginteraction(predinds,preyinds,100,1);
lineartablebuild(sampledpredinds, sampledpreyinds)

R"""
plot($(log.(sampledpredinds)),$(log.(sampledpreyinds)))
abline(linearmodel)
"""


R"""
plot($preyinds,$predinds,log='xy')
"""
sampledpreyinds, sampledpredinds = samplinginteraction(preyinds,predinds,100,10);
lineartablebuild(sampledpreyinds, sampledpredinds)
R"""
plot($(log.(sampledpreyinds)),$(log.(sampledpredinds)))
abline(linearmodel)
"""




## WITH REPLICATIONS



#Do this a bunch of times to get a number of replicates]"
# sizebinsvec = collect(90:100);

sizebins = 100;
reps = 1000;

#Expected prey mass given a predator's mass
ExpPreyreps = Array{Float64}(undef,reps,sizebins,2);

#Expected predator mass given a prey's mass
ExpPredreps = Array{Float64}(undef,reps,sizebins,2);

RawPrey_preds = Array{Float64}(undef,reps,length(preds));
RawPrey_preys = Array{Float64}(undef,reps,length(preds));

for r = 1:reps
    # sizebins = sizebinsvec[r];
    predinds,preyinds,meancarnivore,meanherbivore,meancarnivorediet = genpredprey();

    ## EXPECTED PREY MASS GIVEN PREDATOR MASS
    meanExpPrey_predmass, meanExpPrey_preymass = samplinginteraction(predinds,preyinds,sizebins,1);
    lsp = size(meanExpPrey_preymass)[1];
    #Save sampled predator values (x-axis) column 1
    ExpPreyreps[r,1:lsp,1] = meanExpPrey_predmass;
    #Save sampled prey values (y-axis) column  2
    ExpPreyreps[r,1:lsp,2] = meanExpPrey_preymass;
    ExpPreyreps[r,(lsp+1):sizebins,1] .= NaN;
    ExpPreyreps[r,(lsp+1):sizebins,2] .= NaN;

    ## EXPECTED PREDATOR MASS GIVEN PREY MASS
    meanExpPred_preymass, meanExpPred_predmass = samplinginteraction(preyinds,predinds,sizebins,10);
    lsp = size(meanExpPred_predmass)[1];
    #Save sampled prey values (x-axis) column 1
    ExpPredreps[r,1:lsp,1] = meanExpPred_preymass;
    #Save sampled predator values (y-axis) column 2
    ExpPredreps[r,1:lsp,2] = meanExpPred_predmass;
    ExpPredreps[r,(lsp+1):sizebins,1] .= NaN;
    ExpPredreps[r,(lsp+1):sizebins,2] .= NaN;

    RawPrey_preds[r,:] = meancarnivore;
    RawPrey_preys[r,:] = meancarnivorediet;

end

RawPrey_preds_mean = mean(RawPrey_preds,dims=1);
RawPrey_preys_mean = mean(RawPrey_preys,dims=1);


#REMOVE NAs
ExpPrey_predvec = vec(ExpPreyreps[:,:,1]);
ExpPrey_predvalues = ExpPrey_predvec[findall(!isnan,ExpPrey_predvec)];

ExpPrey_preyvec =  vec(ExpPreyreps[:,:,2]);
ExpPrey_preyvalues = ExpPrey_preyvec[findall(!isnan,ExpPrey_preyvec)];




ExpPred_preyvec = vec(ExpPredreps[:,:,1]);
ExpPred_preyvalues = ExpPred_preyvec[findall(!isnan,ExpPred_preyvec)];
ExpPred_predvec =  vec(ExpPredreps[:,:,2]);
ExpPred_predvalues = ExpPred_predvec[findall(!isnan,ExpPred_predvec)];





scatterplot(log.(ExpPrey_predvalues),log.(ExpPrey_preyvalues))
scatterplot(log.(ExpPred_preyvalues),log.(ExpPred_predvalues))


#Relationship of Expected Prey mass given Predator mass
ExpPrey_fit_table = lineartablebuild(ExpPrey_predvalues,ExpPrey_preyvalues)

R"""
par(mfrow=c(1,2))
plot($(log.(ExpPrey_predvalues)),$(log.(ExpPrey_preyvalues)),pch='.')
abline(linearmodel)
"""

#Relationship of Expected Predator mass given Prey mass
ExpPred_fit_table = lineartablebuild(ExpPred_preyvalues,ExpPred_predvalues)

R"""
plot($(log.(ExpPred_preyvalues)),$(log.(ExpPred_predvalues)),pch='.')
abline(linearmodel)
"""


#EXPORT for the mathematica notebook
# CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPrey_fit_table_revreps.csv",ExpPrey_fit_table; header=true);

# ExpPrey_sizetable = DataFrame([ExpPrey_predvalues ExpPrey_preyvalues],:auto);
# rename!(ExpPrey_sizetable,[:predmass,:preymass]);
# CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPreymass_tablereps.csv",ExpPrey_sizetable; header=false);


# CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPred_fit_table_revreps.csv",ExpPred_fit_table; header=true);

# ExpPred_sizetable = DataFrame([ExpPred_preyvalues ExpPred_predvalues],:auto);
# rename!(ExpPred_sizetable,[:preymass,:predmass]);
# CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPredmass_tablereps.csv",ExpPred_sizetable; header=false);



CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPrey_fit_table_revreps_large.csv",ExpPrey_fit_table; header=true);

ExpPrey_sizetable = DataFrame([ExpPrey_predvalues ExpPrey_preyvalues],:auto);
rename!(ExpPrey_sizetable,[:predmass,:preymass]);
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPreymass_tablereps_large.csv",ExpPrey_sizetable; header=false);


CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPred_fit_table_revreps_large.csv",ExpPred_fit_table; header=true);

ExpPred_sizetable = DataFrame([ExpPred_preyvalues ExpPred_predvalues],:auto);
rename!(ExpPred_sizetable,[:preymass,:predmass]);
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPredmass_tablereps_large.csv",ExpPred_sizetable; header=false);

RawPrey_sizetable = DataFrame([RawPrey_preds_mean' RawPrey_preys_mean'],:auto);
rename!(RawPrey_sizetable,[:predmass,:preymass]);
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/RawPreymass.csv",RawPrey_sizetable; header=false);



# Delong data - RAW DATA
delong_preds = unique(delongdata[!,:Species_name]);
#NOTE: remove ursus arctos - it is matched only with salmon
# delong_preds = delong_preds[delong_preds .!= "Ursus arctos"];
delong_predmass = Array{Float64}(undef,length(delong_preds));
delong_preymass = Array{Float64}(undef,length(delong_preds));
for i=1:length(delong_preds)
    predi = findall(x->x==delong_preds[i],delongdata[!,:Species_name]);
    delong_predmass[i] = mean(delongdata[predi,:Pred_mass_kg]);
    delong_preymass[i] = mean(delongdata[predi,:Prey_mass_kg]);
end

RawPrey_sizetable_delong = DataFrame([delong_predmass delong_preymass],:auto);
rename!(RawPrey_sizetable_delong,[:predmass,:preymass]);
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/RawPreymass_delong.csv",RawPrey_sizetable_delong; header=false);


#Evaluate fit for all MEAN MAMMALIAN DATA (Hayward + Delong)
RawMammalianMean_preds = vec([RawPrey_preds_mean'; delong_predmass]);
RawMammalianMean_prey = vec([RawPrey_preys_mean'; delong_preymass]);
mammalian_fit_table=lineartablebuild(RawMammalianMean_preds,RawMammalianMean_prey);

CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/mammalian_fit_table.csv",mammalian_fit_table; header=true);

Rawmammalian_sizetable = DataFrame([RawMammalianMean_preds RawMammalianMean_prey],:auto);
rename!(Rawmammalian_sizetable,[:predmass,:preymass]);
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/mammalian_mass.csv",Rawmammalian_sizetable; header=false);
