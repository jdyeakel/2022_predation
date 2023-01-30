using(DataFrames)
using(CSV)
using(RCall)
using(LinearAlgebra)
using(Distributions)
using(Distributed)
using(UnicodePlots)

haywardfulldata = CSV.read("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/data_hayward_all.csv",header=true,DataFrame);
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

#Caculcate mean preferred mass per predator
preds = unique(haywardfulldata[!,:Predator]);
# preds = preds[[1,2,3,4,6]]
#SIMULATION FIT
function genpredprey()
preyinds = Array{Float64}(undef,0);
predinds = Array{Float64}(undef,0);
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
    # meanpredmass *= groupsize[i];
    predmassSD = 0.25*meanpredmass;
    predbodysizedist = Normal(meanpredmass,predmassSD);

    #DRAW BODY SIZES FOR PREDATOR INDIVIDUALS
    predinds_draw = abs.(rand(predbodysizedist,Int64(preyind)));
    predinds = [predinds; predinds_draw];
    preyi = haywardfulldata[!,:Prey][findall(x->x==preds[i],haywardfulldata[!,:Predator])];
    
    for j=1:length(preyi)
        if pref[j] > 0.
            #draw body masses
            #Preybodymasskg
            #Preybodymasskg34adultfemalemass
            meanmass = haywardfulldata[!,:Preybodymasskg34adultfemalemass][findall(x->x==preds[i],haywardfulldata[!,:Predator])][j];
            massSD = 0.25*meanmass;
            preybodysizedist = Normal(meanmass,massSD);
            numbers = Int64(pref[j]);
            preyinds_draw = abs.(rand(preybodysizedist,numbers));
            preyinds = [preyinds; preyinds_draw];
        end
    end
end
return(predinds,preyinds)
end

function lineartablebuild(x,y)
#Expected Prey size as a function of predator size
    R"""
    linearmodel = lm(log($y) ~ log($x))
    summary(linearmodel)
    fitintercept = linearmodel[[1]][[1]];
    fitslope = linearmodel[[1]][[2]];
    CI = confint(model3,level=0.95)
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

#General sampling function
function samplinginteraction(xdata,ydata,s,minx)
    xmax = log10(maximum(xdata));
    xmin = log10(minx);
    stepsize = (xmax - xmin)/s;
    xexpbins = collect(xmin:stepsize:xmax);
    meany = Array{Float64}(undef,(length(xexpbins)-1));
    meanx = Array{Float64}(undef,(length(xexpbins)-1));
    for i=2:length(xexpbins)
        xdata_pos = findall(x->((x>10^xexpbins[i-1]) && (x < 10^xexpbins[i])), xdata);
        if length(xdata_pos) > 0
            meany[i-1] = mean(preyinds[xdata_pos]);
            meanx[i-1] = mean([10^xexpbins[i-1],10^xexpbins[i]]);
        else 
            meany[i-1] =NaN;
            meanx[i-1] =NaN;
        end
    end
    filledspots = findall(!isnan,meany);
    return meany[filledspots], meanx[filledspots] #(10 .^predsizeclass[filledspots])
end




predinds,preyinds = genpredprey();

scatterplot(log.(preyinds),log.(predinds))

#Export for the mathematica notebook
fit_table_expprey = lineartablebuild(predinds,preyinds)
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ppmr_expprey_fit_table_rev.csv",fit_table_expprey; header=true);


massdata_expprey = DataFrame([predinds preyinds],:auto);
rename!(massdata_expprey,[:predmass,:preymass])
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPreymass_table.csv",massdata_expprey; header=false);

#####
# Sample the expected prey size across predators in a similar way that we were sampling expected predators given prey size
#####

# Note: the window size does increase (from 1kg to 5 kg) with body size
# I think log-spaced sampling makes more sense
function meanExpPreypredsize(predinds,preyinds,sizebins)
    
    maxpred = log10(maximum(predinds));
    minpred = log10(1);
    stepsize = (maxpred - minpred)/sizebins;
    predsizeclass = collect(minpred:stepsize:maxpred);
    meanpreysize = Array{Float64}(undef,(length(predsizeclass)-1));
    meanpredsize = Array{Float64}(undef,(length(predsizeclass)-1));
    for i=2:length(predsizeclass)
        predsizeinds_pos = findall(x->((x>10^predsizeclass[i-1]) && (x < 10^predsizeclass[i])), predinds);
        if length(predsizeinds_pos) > 0
            meanpreysize[i-1] = mean(preyinds[predsizeinds_pos]);
            meanpredsize[i-1] = mean([10^predsizeclass[i-1],10^predsizeclass[i]]);
        else 
            meanpreysize[i-1] =NaN;
            meanpredsize[i-1] =NaN;
        end
    end

    filledspots = findall(!isnan,meanpreysize);
    return meanpreysize[filledspots], meanpredsize[filledspots] #(10 .^predsizeclass[filledspots])
end


# Note: Linear Spaced sampling
function meanExpPreypredsizeLinear(predinds,preyinds,sizebins)

    #ALTERNATIVE - LINEAR STEP SIZE
    maxpred = maximum(predinds);
    minpred = 1.;
    stepsize = (maxpred - minpred)/sizebins;
    predsizeclass = collect(minpred:stepsize:maxpred);
    meanpreysize = Array{Float64}(undef,(length(predsizeclass)-1));
    meanpredsize = Array{Float64}(undef,(length(predsizeclass)-1));
    for i=2:length(predsizeclass)
        predsizeinds_pos = findall(x->((x>predsizeclass[i-1]) && (x < predsizeclass[i])), predinds);
        if length(predsizeinds_pos) > 0
            meanpreysize[i-1] = mean(preyinds[predsizeinds_pos]);
            meanpredsize[i-1] = mean([predsizeclass[i-1],predsizeclass[i]]);
        else 
            meanpreysize[i-1] =NaN;
            meanpredsize[i-1] =NaN;
        end
    end


    filledspots = findall(!isnan,meanpreysize);
    return meanpreysize[filledspots], meanpredsize[filledspots] #(10 .^predsizeclass[filledspots])
end


#Compare Linear vs. Nonlinear sampling
sizebins = 100;

meanExpPrey_preymass,meanExpPrey_predmass = meanExpPreypredsize(predinds,preyinds,sizebins);
fit_table_meanexpprey = lineartablebuild(meanExpPrey_predmass,meanExpPrey_preymass)

meanExpPrey_preymass2,meanExpPrey_predmass2 = samplinginteraction(predinds,preyinds,sizebins,1);
fit_table_meanexpprey = lineartablebuild(meanExpPrey_predmass2,meanExpPrey_preymass2)

meanExpPrey_preymassLinear,meanExpPrey_predmassLinear = meanExpPreypredsizeLinear(predinds,preyinds,sizebins)



fit_table_meanexppreyLinear = lineartablebuild(meanExpPrey_predmassLinear,meanExpPrey_preymassLinear)


plt = scatterplot(log.(predinds),log.(preyinds))
scatterplot!(plt,log.(meanExpPrey_predmass),log.(meanExpPrey_preymass))

plt = scatterplot(log.(predinds),log.(preyinds))
scatterplot!(plt,log.(meanExpPrey_predmassLinear),log.(meanExpPrey_preymassLinear))

plt = scatterplot(log.(meanExpPrey_predmass),log.(meanExpPrey_preymass))
scatterplot!(plt,log.(meanExpPrey_predmassLinear),log.(meanExpPrey_preymassLinear))





function meanpredpreysize(predinds,preyinds,sizebins)
    maxprey = log10(maximum(preyinds));
    minprey = log10(100); #Run this only for prey = 100 KG to max KG
    # minprey = log10(minimum(preyinds));
    stepsize = (maxprey-minprey)/sizebins;
    preysizeclass = collect(minprey:stepsize:maxprey);
    meanpredsize = Array{Float64}(undef,(length(preysizeclass)-1));
    for i=2:length(preysizeclass)
        preysizeinds_pos = findall(x->((x>10^preysizeclass[i-1]) && (x < 10^preysizeclass[i])), preyinds);
        if length(preysizeinds_pos) > 0
            meanpredsize[i-1] = mean(predinds[preysizeinds_pos]);
        else 
            meanpredsize[i-1] =NaN;
        end
    end
    filledspots = findall(!isnan,meanpredsize)

    return meanpredsize[filledspots], (10 .^preysizeclass[filledspots])
end

sizebins = 100;
meanpredmass,meanpreymass = meanpredpreysize(predinds,preyinds,sizebins)
meanpredmass


#Expected predator size as a function of prey size
R"""
model4 = lm(log($(meanpredmass)) ~ log($(meanpreymass)))
summary(model4)
fitintercept = model4[[1]][[1]];
fitslope = model4[[1]][[2]];
CI = confint(model4,level=0.95)
intlow = CI[[1]];
slopelow = CI[[2]];
inthigh = CI[[3]];
slopehigh = CI[[4]];
"""
fitintercept = @rget fitintercept;
fitslope = @rget fitslope;
fitinterceptlow = @rget intlow;
fitslopelow = @rget slopelow;
fitintercepthigh = @rget inthigh;
fitslopehigh = @rget slopehigh;
#Export for the mathematica notebook
fit_table = DataFrame([fitintercept fitinterceptlow fitintercepthigh; fitslope fitslopelow fitslopehigh], :auto);
rename!(fit_table,[:Fit,:FitLow,:FitHigh])

CSV.write("$(homedir())/Dropbox/PostDoc/2021_TaranWebs/data/ppmr_fit_table_rev.csv",fit_table; header=true);

predpreysizetable = DataFrame([meanpreymass meanpredmass],:auto);
rename!(predpreysizetable,[:preymass,:predmass])
CSV.write("$(homedir())/Dropbox/PostDoc/2021_TaranWebs/data/predpreymass_table.csv",predpreysizetable; header=false);

scatterplot(log.(meanpreymass),log.(meanpredmass))

namespace = string(homedir(),"/Dropbox/PostDoc/2021_TaranWebs/figures/fig_ppmr.pdf")
R"""
pdf($namespace,height=5,width=6)
plot(log($(meanpreymass)),log($(meanpredmass)),pch=16,col='black',xlim=c(4.5,8.5),ylim=c(4.5,6),xlab='Prey mass (kg)',ylab='Mean predator mass (kg)',cex=0.5)
lines(seq(0,3000),seq(0,3000),lty=2)
abline(model4,col='blue')
# lines(seq(1,10),inthigh+slopehigh*seq(1,10),col='red')
dev.off()
"""



#Do this a bunch of times to get a number of replicates]"
sizebinsvec = collect(90:100);

sizebinsvec = repeat([100],inner=1000)
reps = length(sizebinsvec);

preypredreps = Array{Float64}(undef,reps,maximum(sizebinsvec),2);
predpreyreps = Array{Float64}(undef,reps,maximum(sizebinsvec),2);

for r = 1:reps
    sizebins = sizebinsvec[r];
    predinds,preyinds = genpredprey();
    
    meanExpPrey_preymass,meanExpPrey_predmass = meanExpPreypredsize(predinds,preyinds,sizebins);
    lsp = size(meanExpPrey_preymass)[1];
    preypredreps[r,1:lsp,1] = meanExpPrey_predmass;
    preypredreps[r,1:lsp,2] = meanExpPrey_preymass;
    preypredreps[r,(lsp+1):maximum(sizebinsvec),1] .= NaN;
    preypredreps[r,(lsp+1):maximum(sizebinsvec),2] .= NaN;

    meanpredmass,meanpreymass = meanpredpreysize(predinds,preyinds,sizebins);
    lsp = size(meanpredmass)[1];
    predpreyreps[r,1:lsp,1] = meanpreymass;
    predpreyreps[r,1:lsp,2] = meanpredmass;

    predpreyreps[r,(lsp+1):maximum(sizebinsvec),1] .= NaN;
    predpreyreps[r,(lsp+1):maximum(sizebinsvec),2] .= NaN;
end

ExpPrey_predvec = vec(preypredreps[:,:,1]);
ExpPrey_predvalues = ExpPrey_predvec[findall(!isnan,ExpPrey_predvec)];
ExpPrey_preyvec =  vec(preypredreps[:,:,2]);
ExpPrey_preyvalues = ExpPrey_preyvec[findall(!isnan,ExpPrey_preyvec)];

scatterplot(log.(ExpPrey_predvalues),log.(ExpPrey_preyvalues))


preyvec = vec(predpreyreps[:,:,1]);
preyvalues = preyvec[findall(!isnan,preyvec)];
predvec =  vec(predpreyreps[:,:,2]);
predvalues = predvec[findall(!isnan,predvec)];


scatterplot(log.(preyvalues),log.(predvalues))




#Expected Prey size as a function of predator size
R"""
model3 = lm(log($preyvalues) ~ log($predvalues))
summary(model3)
fitintercept_expprey = model3[[1]][[1]];
fitslope_expprey = model3[[1]][[2]];
CI = confint(model3,level=0.95)
intlow_expprey = CI[[1]];
slopelow_expprey = CI[[2]];
inthigh_expprey = CI[[3]];
slopehigh_expprey = CI[[4]];
"""
fitintercept_expprey = @rget fitintercept_expprey;
fitslope_expprey = @rget fitslope_expprey;
intlow_expprey = @rget intlow_expprey;
slopelow_expprey = @rget slopelow_expprey;
inthigh_expprey = @rget inthigh_expprey;
slopehigh_expprey = @rget slopehigh_expprey;


#Export for the mathematica notebook
fit_table_expprey = DataFrame([fitintercept_expprey intlow_expprey inthigh_expprey; fitslope_expprey slopelow_expprey slopehigh_expprey], :auto);
rename!(fit_table_expprey,[:Fit,:FitLow,:FitHigh])

CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ppmr_expprey_fit_table_revreps.csv",fit_table_expprey; header=true);


ExpPreysizetable = DataFrame([predvalues preyvalues],:auto);
rename!(ExpPreysizetable,[:predmass,:preymass])
CSV.write("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/ExpPreymass_tablereps.csv",ExpPreysizetable; header=false);




R"""
model5 = lm(log($(predvalues)) ~ log($(preyvalues)))
summary(model5)
fitintercept = model5[[1]][[1]];
fitslope = model5[[1]][[2]];
CI = confint(model5,level=0.95)
intlow = CI[[1]];
slopelow = CI[[2]];
inthigh = CI[[3]];
slopehigh = CI[[4]];
"""
fitintercept = @rget fitintercept;
fitslope = @rget fitslope;
fitinterceptlow = @rget intlow;
fitslopelow = @rget slopelow;
fitintercepthigh = @rget inthigh;
fitslopehigh = @rget slopehigh;
#Export for the mathematica notebook
fit_table = DataFrame([fitintercept fitinterceptlow fitintercepthigh; fitslope fitslopelow fitslopehigh], :auto);
rename!(fit_table,[:Fit,:FitLow,:FitHigh])

CSV.write("$(homedir())/Dropbox/PostDoc/2021_TaranWebs/data/ppmr_fit_table_revreps.csv",fit_table; header=true);

predpreysizetable = DataFrame([preyvalues predvalues],:auto);
rename!(predpreysizetable,[:preymass,:predmass])
CSV.write("$(homedir())/Dropbox/PostDoc/2021_TaranWebs/data/predpreymass_tablereps.csv",predpreysizetable; header=false);





namespace = string(homedir(),"/Dropbox/PostDoc/2021_TaranWebs/figures/fig_ppmr_reps.pdf")
R"""
pdf($namespace,height=5,width=6)
plot(log($(preyvalues)),log($(predvalues)),pch='.',col='black',xlim=c(4.5,8.5),ylim=c(4.5,6),xlab='Prey mass (kg)',ylab='Mean predator mass (kg)',cex=0.5)
lines(seq(0,3000),seq(0,3000),lty=2)
abline(model5,col='blue')
# lines(seq(1,10),inthigh+slopehigh*seq(1,10),col='red')
dev.off()
"""



#Fit to certain prey weight range
minpreymass = 500;
massloc = findall(x->x>minpreymass,predpreysizetable[!,:preymass])
R"""
model5 = lm(log($(meanpredmass[massloc])) ~ log($(meanpreymass[massloc])))
summary(model5)
fitintercept = model5[[1]][[1]];
fitslope = model5[[1]][[2]];
CI = confint(model5,level=0.95)
intlow = CI[[1]];
slopelow = CI[[2]];
inthigh = CI[[3]];
slopehigh = CI[[4]];
"""
fitintercept = @rget fitintercept;
fitslope = @rget fitslope;
fitinterceptlow = @rget intlow;
fitslopelow = @rget slopelow;
fitintercepthigh = @rget inthigh;
fitslopehigh = @rget slopehigh;
fit_table_large = DataFrame([fitintercept fitinterceptlow fitintercepthigh; fitslope fitslopelow fitslopehigh]);
rename!(fit_table_large,[:Fit,:FitLow,:FitHigh])
