%%%%%%%%%%%%%%% Extract two components of the dendrometers curves : GRO
%%%%%%%%%%%%%%% (irreversible growth) and TWD (tree water deficit i.e.
%%%%%%%%%%%%%%% shrinkage and swelling; reversible) 

% Author: Audrey Etienne
% Date: 31/08/2017
% contact mail: audrey.etienne@syngenta.com


% see Zweifel et al., 2016

% load dendrometer data
load('C:\Users\t957509\OneDrive - Syngenta\Documents\cocoa_project\brazil_field_data\dendrometer_data\compil_dendro_data.mat')


%%
% vector of good dendrometers (I keep only dendrometers having a nice
% behavior)
good_dendro = [532, 531, 547, 548, 549, 550, 552, 553, 554, 555, 592, 593, 595, 214, 211, 213, 215, 193, 192, 191, 346, 347, 348];

% define preallocated matrix to store result
GRO_dendro = nan(size(compildendrodataS2,1),length(good_dendro));
TWD_dendro = nan(size(compildendrodataS2,1),length(good_dendro));
cumGRO_dendro = zeros(size(compildendrodataS2,1),length(good_dendro));


for i = 1:length(good_dendro)
    
    col_id = good_dendro(i);
    formatSpec = 'Dendro_%d';
    str = sprintf(formatSpec,col_id);
    
    y = compildendrodataS2{:,str};
    
    for t = 1:size(compildendrodataS2,1)
        
        % GRO (irreversible growth)
        if y(t) >= max(y(1:t-1))
            GRO_dendro(t,i) = y(t) - max(y(1:t-1));
        else
            GRO_dendro(t,i) = 0;
        end
        
        % TWD (tree water deficit)
        if y(t) < max(y(1:t-1))
            TWD_dendro(t,i) = max(y(1:t-1)) - y(t);
        else
            TWD_dendro(t,i) = 0;
        end
        
    end
        
    % compute cumsum of GRO (cumulative net growth)
    for t = 2 : size(GRO_dendro,1)       
      cumGRO_dendro(t,i) = cumGRO_dendro(t-1,i) + GRO_dendro(t,i);        
    end
    
end

% create table to store results
x = datetime(compildendrodataS2.year, compildendrodataS2.month, compildendrodataS2.day, compildendrodataS2.hour, compildendrodataS2.min, compildendrodataS2.sec);

GRO = table(x,GRO_dendro(:,1),GRO_dendro(:,2),GRO_dendro(:,3),GRO_dendro(:,4),GRO_dendro(:,5),GRO_dendro(:,6),GRO_dendro(:,7),GRO_dendro(:,8),GRO_dendro(:,9),...
    GRO_dendro(:,10),GRO_dendro(:,11),GRO_dendro(:,12),GRO_dendro(:,13),GRO_dendro(:,14),GRO_dendro(:,15),GRO_dendro(:,16),GRO_dendro(:,17),GRO_dendro(:,18),GRO_dendro(:,19),...
    GRO_dendro(:,20),GRO_dendro(:,21),GRO_dendro(:,22),GRO_dendro(:,23),...
    'VariableNames',{'DateTime','Dendro_532', 'Dendro_531', 'Dendro_547', 'Dendro_548', 'Dendro_549', 'Dendro_550', 'Dendro_552', 'Dendro_553', 'Dendro_554', 'Dendro_555', 'Dendro_592', 'Dendro_593', 'Dendro_595', 'Dendro_214', 'Dendro_211', 'Dendro_213', 'Dendro_215', 'Dendro_193', 'Dendro_192', 'Dendro_191', 'Dendro_346', 'Dendro_347', 'Dendro_348'});

TWD = table(x,TWD_dendro(:,1),TWD_dendro(:,2),TWD_dendro(:,3),TWD_dendro(:,4),TWD_dendro(:,5),TWD_dendro(:,6),TWD_dendro(:,7),TWD_dendro(:,8),TWD_dendro(:,9),...
    TWD_dendro(:,10),TWD_dendro(:,11),TWD_dendro(:,12),TWD_dendro(:,13),TWD_dendro(:,14),TWD_dendro(:,15),TWD_dendro(:,16),TWD_dendro(:,17),TWD_dendro(:,18),TWD_dendro(:,19),...
    TWD_dendro(:,20),TWD_dendro(:,21),TWD_dendro(:,22),TWD_dendro(:,23),...
    'VariableNames',{'DateTime','Dendro_532', 'Dendro_531', 'Dendro_547', 'Dendro_548', 'Dendro_549', 'Dendro_550', 'Dendro_552', 'Dendro_553', 'Dendro_554', 'Dendro_555', 'Dendro_592', 'Dendro_593', 'Dendro_595', 'Dendro_214', 'Dendro_211', 'Dendro_213', 'Dendro_215', 'Dendro_193', 'Dendro_192', 'Dendro_191', 'Dendro_346', 'Dendro_347', 'Dendro_348'});


cumGRO = table(x,cumGRO_dendro(:,1),cumGRO_dendro(:,2),cumGRO_dendro(:,3),cumGRO_dendro(:,4),cumGRO_dendro(:,5),cumGRO_dendro(:,6),cumGRO_dendro(:,7),cumGRO_dendro(:,8),cumGRO_dendro(:,9),...
    cumGRO_dendro(:,10),cumGRO_dendro(:,11),cumGRO_dendro(:,12),cumGRO_dendro(:,13),cumGRO_dendro(:,14),cumGRO_dendro(:,15),cumGRO_dendro(:,16),cumGRO_dendro(:,17),cumGRO_dendro(:,18),cumGRO_dendro(:,19),...
    cumGRO_dendro(:,20),cumGRO_dendro(:,21),cumGRO_dendro(:,22),cumGRO_dendro(:,23),...
    'VariableNames',{'DateTime','Dendro_532', 'Dendro_531', 'Dendro_547', 'Dendro_548', 'Dendro_549', 'Dendro_550', 'Dendro_552', 'Dendro_553', 'Dendro_554', 'Dendro_555', 'Dendro_592', 'Dendro_593', 'Dendro_595', 'Dendro_214', 'Dendro_211', 'Dendro_213', 'Dendro_215', 'Dendro_193', 'Dendro_192', 'Dendro_191', 'Dendro_346', 'Dendro_347', 'Dendro_348'});


