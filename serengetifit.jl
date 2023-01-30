using(DataFrames)
using(CSV)
using(RCall)
using(LinearAlgebra)
using(Distributions)
using(Arpack)
using(Optim)
using(Distributed)
include("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/src/laplacian.jl")
include("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/src/eigencluster.jl")
include("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/src/eigendistance.jl")
include("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/src/logitlikelihood.jl")
include("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/src/lfunc.jl")
include("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/src/fc.jl")


pcdata = CSV.read("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/pc_bones.csv",header=true,DataFrame);
sp = pcdata[!,:common_name];
genus = pcdata[!,:Genus];
species = pcdata[!,:Species];
_array = Array{Char}(undef,length(genus)); _array[1:length(genus)] .= ' ';
gensp = mapslices(join, [Array{String}(genus) _array Array{String}(species)], dims=2);
nsp = size(pcdata)[1];
nmeas = size(pcdata)[2];

#Archive body mass measure
bodymass = Array(pcdata[!,:mass_max_kg]);
#take out body mass data info
remove = [findall(x->x==:mass_max_kg,names(pcdata));findall(x->x==:mass_min_kg,names(pcdata))]
keep = deleteat!(collect(1:nmeas),remove);
pcdata = pcdata[:,keep];
nmeas = size(pcdata)[2];
pcdatatr = copy(pcdata[:,6:nmeas]);
#Delete measurements that are missing for ALL species
nmeas = size(pcdatatr)[2];
todelete = Array{Int64}(undef,0);
for i=1:nmeas
    if all(ismissing.(pcdatatr[:,i]))
        push!(todelete,i);
    end
end
pcdatatr = pcdatatr[:,setdiff(collect(1:nmeas),todelete)];
meas = Array{String}(String.(names(pcdatatr)));
nmeas = size(pcdatatr)[2];


baskspcodes = CSV.read("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/Baskerville_Table_S1.csv",header=true,DataFrame);
baskcode = Array(baskspcodes[!,:code]);
baskgensp = Array(baskspcodes[!,:species]);


basktrophic = CSV.read("$(homedir())/Dropbox/PostDoc/2022_PredatorConsumerResource/data/Baskerville_Table_S2.csv",header=true,DataFrame);
preds = basktrophic[!,:pred_code];
preys = basktrophic[!,:prey_code];
npreds = length(preds);

trophic = Array{String}(undef,length(preds),2);
predsunique = unique(preds);
preysunique = unique(preys);
for i = 1:length(predsunique)
    id = findall(x->x==predsunique[i],preds);
    codetosp = findall(x->x==predsunique[i],baskcode);
    trophic[id,1] = repeat(baskgensp[codetosp],outer=length(id));
end
for i = 1:length(preysunique)
    id = findall(x->x==preysunique[i],preys);
    codetosp = findall(x->x==preysunique[i],baskcode);
    trophic[id,2] = repeat(baskgensp[codetosp],outer=length(id));
end
trophiclevel = [repeat([2],inner=9);repeat([1],inner=(32-9))]
#Include only consumers in the serengeti system
spnames = unique(trophic[:,1]);
SerengetiAdj = zeros(Int64,32,9);
for i=1:9
    predloc = findall(x->x==spnames[i],trophic[:,1])
    preyid = trophic[predloc,2];
    preyloc = Array{Int64}(undef,0);
    for j=1:length(preyid)
        sp_position = findall(x->x==preyid[j],spnames)[1];
        push!(preyloc,sp_position)
    end
    SerengetiAdj[preyloc,i] .= 1;
end


#Mass vector
massvec = Array{Float64}(undef,32);
for i=1:32
    spid = spnames[i];
    masspos = findall(x->x==spid,vec(gensp));
    if length(masspos) > 0
        massvec[i] = bodymass[masspos[1]];
    else
        massvec[i] = 0;
    end
end

# Missing masses
# "Canis aureus"
# "Damaliscus korrigum"
# "Heterohyrax brucei"
# "Papio anubis"
# "Pedetes capensis"
# "Procavia capensis"
# "Rhabdomys pumilio"
missingmass = [10,108,2.4,20,3.5,3.8,0.0397];
massvec[findall(iszero,massvec)] .= missingmass;


#Assess logit parameters


## Serengeti
x0 = [0.0,0.0,0.0];
results_ser = optimize(x->lfunc(x,SerengetiAdj,massvec),x0,NelderMead());
results_ser.minimizer
xmax = results_ser.minimizer;
fcorr_bg, Apredict_ser = fc(xmax,SerengetiAdj,massvec)




predtopreyratio = exp(xmax[2]/(2*xmax[3]))

