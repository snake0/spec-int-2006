% rcsmain.m
% 
% Written by Ulf Andersson 99-02-08, modified by Åke Rydell 99-10-19.
% 
% Plots the monostatic RCS of a sphere. 
% Analytical and computational solutions are compared.
% 

clear, clf
format compact

% Control parameters
lw = 2;     % Width of plotted lines
asize = 14; % Font size of axis Ticks
lsize = 14; % Font size of axis labels
tsize = 12; % Font size of title string

% Analytical solution

load sphere.mat % (a = 0.205)
plot(1E-9*f,sigma,'b-','LineWidth',lw), hold on 
A = pi*a^2; normA = 10*log10(A);

% Computational solutions
% help farfieldgemsTD is instructive

% NFT:-----------------------------------------------------------------------

filename_nft = 'sphere_td.nft';
if exist(filename_nft)==2  % Check if the solution file exists.
  [Einf_td,Ethf_td,Ephif_td,f_td,Etht_td,Ephit_td,Nfreq,ntstep,toffset,dt] ...
      = farfieldgemsTD(filename_nft);

  Ethf_td(1,:) = 0.00001 + i*0.00001; %To avoid taking log of zero (dc comp.)

  % Plot normalized RCS
  
  plot(1E-9*f_td,10*log10(4*pi*(abs(Ethf_td(:,1))./ ...
                                abs(Einf_td(:))).^2)-normA, ...
				              'k--', 'LineWidth',lw')
			  
else
  disp(['file ' filename_nft ' not found'])
  
end

% ---------------------------------------------------------------------------

legend('Anal. sol.', 'Comp. sol.')

ht = title('Monostatic RCS of a PEC sphere (ka=4.3 for f=1GHz)');
hx = xlabel('f (GHz)');
hy = ylabel('\sigma / \pi a^2 [dB]');
grid on

% Polish the plot
set(gca, 'FontSize', asize)
set(hx,  'FontSize', lsize)
set(hy,  'FontSize', lsize)
set(ht,  'FontSize', tsize)
axis([0 2 -25 10])

return


