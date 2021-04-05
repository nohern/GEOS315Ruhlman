%file to create bar graphs 

load XEPOSDataRealFinalFormat
SampleData = table2array(XEPOSDataRealFinalFormat(:, 2:15));
SurveyData = XEPOSDataRealFinalFormat(:, 16:30);

Si=1; %Si column # (Sample Data)
P=2; %P column #
S=3; %S column #
Ca=4; %Ca column #
Ti=5; %Ti column #
Fe=6; %Fe column #
Ni=7; %Ni column #
Cu=8; %Cu coulmn #
Zn=9; %Zn column #
Br=10;%Br column #
Sr=11; %Sr column #
Mo=12; %Mo column #
Sb=13; %Sb column #
Pb=14; %Pb column #

EOInumb= size(SampleData, 2);
x = 8 ; % Column number of Hair Texture (SurveyData)
y = 14; % Column number of Product Type
z= 4; %Column number of diet
w=6; %column number of chemical treatment

%Sort by Hair Texture
HairTexture = table2array(SurveyData(:,x));
WavyHair = [] ;
CurlyHair = [];
StraightHair = [];
UnknownHair = [];

for i= 1:size(SampleData, 1)
    if HairTexture(i) == 'Wavy';
        WavyHair = [WavyHair; SampleData(i, :)];
    elseif HairTexture(i) == 'Curly';
        CurlyHair = [CurlyHair; SampleData(i, :)];
    elseif HairTexture(i) == 'Straight';
        StraightHair = [StraightHair; SampleData(i, :)];
    else HairTexture(i) == 'Unknown';
        UnknownHair = [UnknownHair; SampleData(i, :)];
    end
end

%Get Avgs for Hair Texture
HairTextureAvgs ={};
HairTextureStd= {};
HairTextureSorted= {CurlyHair; StraightHair; UnknownHair; WavyHair};
for i= 1: size(HairTextureSorted)
        for j=  1: size(HairTextureSorted{i,1},2)
             HairTextureAvgs{i,1}(1, j)= mean(HairTextureSorted{i,1}(:,j), 'omitnan');
             HairTextureStd{i,1}(1, j)= std(HairTextureSorted{i,1}(:,j), 'omitnan');
        end        
end

HairTextureAvgs = cell2mat(HairTextureAvgs);
HairTextureStd= cell2mat(HairTextureStd);
HairTextureTypes = categorical({'Curly'; 'Straight' ; 'Unknown'; 'Wavy'}); %put in alphabetic order

Colors= {[0 0 1], [1 0 1], [0.3010 0.7450 0.9330], [1 0 0], [0 1 1], [.5 0 .5],  [0.6350 0.0780 0.1840], [0 0.4470 0.7410]};

%TraceMetals:  [Zn, Fe, Cu, Ni]
TraceMetalIndex= [Zn, Fe, Ni, Cu];
TraceMetalNames= {'Zn', 'Fe', 'Ni', 'Cu'};
figure 
sgtitle('Hair Texture Types-Trace Metals');
for i= 1: length(TraceMetalIndex)
    subplot(2,2,i) 
    hold on
    b= bar(HairTextureTypes, HairTextureAvgs(:,TraceMetalIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    b.CData(4,:) = Colors{4};
    title(TraceMetalNames(i))
    ylabel('Conc.(ppm)')
    err=HairTextureStd(:, TraceMetalIndex(i));
    errorbar(1:length(HairTextureTypes), HairTextureAvgs(:,TraceMetalIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Matrix Elements: [Ca, Ti, S, Si, P, Sr, Br]
MatrixElementsIndex= [Ca, Ti, S, Si, P, Sr, Br];
MatrixElementsNames= {'Ca', 'Ti', 'S', 'Si', 'P', 'Sr', 'Br'};
figure 
sgtitle('Hair Texture Types- Matrix Elements');
for i= 1: length(MatrixElementsIndex)
    subplot(3,3,i) 
    hold on
    b= bar(HairTextureTypes, HairTextureAvgs(:,MatrixElementsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    b.CData(4,:) = Colors{4};
    title(MatrixElementsNames(i))
    ylabel('Conc.(ppm)')
    err=HairTextureStd(:, MatrixElementsIndex(i));
    errorbar(1:length(HairTextureTypes), HairTextureAvgs(:,MatrixElementsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Heavy Metals:  [Pb, Sb]
HeavyMetalsIndex= [Pb, Sb];
HeavyMetalNames= {'Pb', 'Sb'};
figure 
sgtitle('Hair Texture Types- Heavy Metals');
for i= 1: length(HeavyMetalsIndex)
    subplot(2,1,i) 
    hold on
    b=bar(HairTextureTypes, HairTextureAvgs(:,HeavyMetalsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    b.CData(4,:) = Colors{4};
    title(HeavyMetalNames(i))
    ylabel('Conc.(ppm)')
    err=HairTextureStd(:, HeavyMetalsIndex(i));
    errorbar(1:length(HairTextureTypes), HairTextureAvgs(:,HeavyMetalsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end


%Sort by ProductType
ProductType = table2array(SurveyData(:,y));
AntiDandruff = [] ;
AntiFrizz = [];
OtherProduct = [];


for i= 1:size(SampleData, 1)
    if ProductType(i) == 'Anti-Dandruff';
        AntiDandruff = [AntiDandruff; SampleData(i, :)];
    elseif ProductType(i) == 'Anti-Frizz';
        AntiFrizz = [AntiFrizz; SampleData(i, :)];
    else ProductType(i) == 'Other';
        OtherProduct = [OtherProduct; SampleData(i, :)];
    end
end

%Get Avgs for Hair Texture
ProductTypeAvgs ={};
ProductTypeStd= {};
ProductTypesSorted= {AntiDandruff; AntiFrizz; OtherProduct};
for i= 1: size(ProductTypesSorted)
        for j=  1: size(ProductTypesSorted{i,1},2)
             ProductTypeAvgs{i,1}(1, j)= mean(ProductTypesSorted{i,1}(:,j), 'omitnan');
             ProductTypeStd{i,1}(1, j)= std(ProductTypesSorted{i,1}(:,j), 'omitnan');
        end        
end

ProductTypeAvgs = cell2mat(ProductTypeAvgs);
ProductTypeStd= cell2mat(ProductTypeStd);
ProductTypeTypes = categorical({'Anti-Dandruff'; 'Anti-Frizz' ; 'Other'}); %put in alphabetic order

%TraceMetals:  [Zn, Fe, Cu, Ni]
TraceMetalIndex= [Zn, Fe, Ni, Cu];
TraceMetalNames= {'Zn', 'Fe', 'Ni', 'Cu'};
figure 
sgtitle('Product Types Used -Trace Metals');
for i= 1: length(TraceMetalIndex)
    subplot(2,2,i) 
    hold on
    b=bar(ProductTypeTypes, ProductTypeAvgs(:,TraceMetalIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(TraceMetalNames(i))
    ylabel('Conc.(ppm)')
    err=ProductTypeStd(:, TraceMetalIndex(i));
    errorbar(1:length(ProductTypeTypes), ProductTypeAvgs(:,TraceMetalIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Matrix Elements: [Ca, Ti, S, Si, P, Sr, Br]
MatrixElementsIndex= [Ca, Ti, S, Si, P, Sr, Br];
MatrixElementsNames= {'Ca', 'Ti', 'S', 'Si', 'P', 'Sr', 'Br'};
figure 
sgtitle('Product Types Used- Matrix Elements');
for i= 1: length(MatrixElementsIndex)
    subplot(3,3,i) 
    hold on
    b=bar(ProductTypeTypes, ProductTypeAvgs(:,MatrixElementsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(MatrixElementsNames(i))
    ylabel('Conc.(ppm)')
    err=ProductTypeStd(:, MatrixElementsIndex(i));
    errorbar(1:length(ProductTypeTypes), ProductTypeAvgs(:,MatrixElementsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Heavy Metals:  [Pb, Sb]
HeavyMetalsIndex= [Pb, Sb];
HeavyMetalNames= {'Pb', 'Sb'};
figure 
sgtitle('Product Types Used- Heavy Metals');
for i= 1: length(HeavyMetalsIndex)
    subplot(2,1,i) 
    hold on
    b=bar(ProductTypeTypes, ProductTypeAvgs(:,HeavyMetalsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(HeavyMetalNames(i))
    ylabel('Conc.(ppm)')
    err=ProductTypeStd(:, HeavyMetalsIndex(i));
    errorbar(1:length(ProductTypeTypes), ProductTypeAvgs(:,HeavyMetalsIndex(i)), err, 'k',  'linestyle', 'none');
    hold off
end

%Sort by Diet
Diet = table2array(SurveyData(:,z));
VeganVegetarian = [] ;
ReducedMeat = [];
AllFoods = [];


for i= 1:size(SampleData, 1)
    if Diet(i) == 'Vegan/Vegetarian';
        VeganVegetarian = [VeganVegetarian; SampleData(i, :)];
    elseif Diet(i) == 'Reduced Meat';
        ReducedMeat = [ReducedMeat; SampleData(i, :)];
    else Diet(i) == 'All Foods';
        AllFoods = [AllFoods; SampleData(i, :)];
    end
end

%Get Avgs for Hair Texture
DietAvgs ={};
DietStd= {};
DietSorted= {AllFoods; ReducedMeat; VeganVegetarian;};
for i= 1: size(DietSorted)
        for j=  1: size(DietSorted{i,1},2)
             DietAvgs{i,1}(1, j)= mean(DietSorted{i,1}(:,j), 'omitnan');
             DietStd{i,1}(1, j)= std(DietSorted{i,1}(:,j), 'omitnan');
        end        
end

DietAvgs = cell2mat(DietAvgs);
DietStd= cell2mat(DietStd);
DietTypes = categorical({'All Foods'; 'Reduced Meat' ; 'Vegan/Vegetarian'}); %put in alphabetic order

%TraceMetals:  [Zn, Fe, Cu, Ni]
TraceMetalIndex= [Zn, Fe, Ni, Cu];
TraceMetalNames= {'Zn', 'Fe', 'Ni', 'Cu'};
figure 
sgtitle('Diet -Trace Metals');
for i= 1: length(TraceMetalIndex)
    subplot(2,2,i) 
    hold on
    b=bar(DietTypes, DietAvgs(:,TraceMetalIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(TraceMetalNames(i))
    ylabel('Conc.(ppm)')
    err=DietStd(:, TraceMetalIndex(i));
    errorbar(1:length(DietTypes), DietAvgs(:,TraceMetalIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Matrix Elements: [Ca, Ti, S, Si, P, Sr, Br]
MatrixElementsIndex= [Ca, Ti, S, Si, P, Sr, Br];
MatrixElementsNames= {'Ca', 'Ti', 'S', 'Si', 'P', 'Sr', 'Br'};
figure 
sgtitle('Diet- Matrix Elements');
for i= 1: length(MatrixElementsIndex)
    subplot(3,3,i) 
    hold on
    b=bar(DietTypes, DietAvgs(:,MatrixElementsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(MatrixElementsNames(i))
    ylabel('Conc.(ppm)')
    err=DietStd(:, MatrixElementsIndex(i));
    errorbar(1:length(DietTypes), DietAvgs(:,MatrixElementsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Heavy Metals:  [Pb, Sb]
HeavyMetalsIndex= [Pb, Sb];
HeavyMetalNames= {'Pb', 'Sb'};
figure 
sgtitle('Diet- Heavy Metals');
for i= 1: length(HeavyMetalsIndex)
    subplot(2,1,i) 
    hold on
    b=bar(DietTypes, DietAvgs(:,HeavyMetalsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(HeavyMetalNames(i))
    ylabel('Conc.(ppm)')
    err=DietStd(:, HeavyMetalsIndex(i));
    errorbar(1:length(DietTypes), DietAvgs(:,HeavyMetalsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end


%Sort by Chemical Treatment
ChemicalTreatment = table2array(SurveyData(:,w));
Dye = [] ;
None = [];
Perm = [];


for i= 1:size(SampleData, 1)
    if ChemicalTreatment(i) == 'Bleach/Dye';
        Dye = [Dye; SampleData(i, :)];
    elseif ChemicalTreatment(i) == 'None';
        None = [None; SampleData(i, :)];
    else ChemicalTreatment(i) == 'Perm/Straightener';
        Perm = [Perm; SampleData(i, :)];
    end
end

%Get Avgs for Hair Texture
ChemicalTreatmentAvgs ={};
ChemicalTeatmentStd= {};
DietSorted= {Dye; None; Perm};
for i= 1: size(DietSorted)
        for j=  1: size(DietSorted{i,1},2)
             ChemicalTreatmentAvgs{i,1}(1, j)= mean(DietSorted{i,1}(:,j), 'omitnan');
             ChemicalTeatmentStd{i,1}(1, j)= std(DietSorted{i,1}(:,j), 'omitnan');
        end        
end

ChemicalTreatmentAvgs = cell2mat(ChemicalTreatmentAvgs);
ChemicalTeatmentStd= cell2mat(ChemicalTeatmentStd);
ChemicalTreatmentTypes = categorical({'Dye/Bleach'; 'None' ; 'Perm/Straightener'}); %put in alphabetic order

%TraceMetals:  [Zn, Fe, Cu, Ni]
TraceMetalIndex= [Zn, Fe, Ni, Cu];
TraceMetalNames= {'Zn', 'Fe', 'Ni', 'Cu'};
figure 
sgtitle('Chemical Treatment -Trace Metals');
for i= 1: length(TraceMetalIndex)
    subplot(2,2,i) 
    hold on
    b=bar(ChemicalTreatmentTypes, ChemicalTreatmentAvgs(:,TraceMetalIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(TraceMetalNames(i))
    ylabel('Conc.(ppm)')
    err=ChemicalTeatmentStd(:, TraceMetalIndex(i));
    errorbar(1:length(ChemicalTreatmentTypes), ChemicalTreatmentAvgs(:,TraceMetalIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Matrix Elements: [Ca, Ti, S, Si, P, Sr, Br]
MatrixElementsIndex= [Ca, Ti, S, Si, P, Sr, Br];
MatrixElementsNames= {'Ca', 'Ti', 'S', 'Si', 'P', 'Sr', 'Br'};
figure 
sgtitle('Chemical Treatment- Matrix Elements');
for i= 1: length(MatrixElementsIndex)
    subplot(3,3,i) 
    hold on
    b=bar(ChemicalTreatmentTypes, ChemicalTreatmentAvgs(:,MatrixElementsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(MatrixElementsNames(i))
    ylabel('Conc.(ppm)')
    err=ChemicalTeatmentStd(:, MatrixElementsIndex(i));
    errorbar(1:length(ChemicalTreatmentTypes), ChemicalTreatmentAvgs(:,MatrixElementsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%Heavy Metals:  [Pb, Sb]
HeavyMetalsIndex= [Pb, Sb];
HeavyMetalNames= {'Pb', 'Sb'};
figure 
sgtitle('Chemical Treatment- Heavy Metals');
for i= 1: length(HeavyMetalsIndex)
    subplot(2,1,i) 
    hold on
    b=bar(ChemicalTreatmentTypes, ChemicalTreatmentAvgs(:,HeavyMetalsIndex(i)));%,'FaceColor', Colors{i});
    b.FaceColor = 'flat';
    b.CData(1,:) = Colors{1};
    b.CData(2,:) = Colors{2};
    b.CData(3,:) = Colors{3};
    title(HeavyMetalNames(i))
    ylabel('Conc.(ppm)')
    err=ChemicalTeatmentStd(:, HeavyMetalsIndex(i));
    errorbar(1:length(ChemicalTreatmentTypes), ChemicalTreatmentAvgs(:,HeavyMetalsIndex(i)), err, 'k', 'linestyle', 'none');
    hold off
end

%to get individual plots
%for j=[a,b,c,d,e,f,g,h,i,j,k,l,m,n]
    %figure
    %bar(HairTextureTypes, HairTextureAvgs(:,j));
    %title(names(j));
%end



        


