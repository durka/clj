function figs = plot_hohmann(hoh)
    figs = [figure, figure, figure];
    
    % plot radius and burns as functions of time
    figure(figs(2));
    plot(hoh(:,1), sqrt(hoh(:,6).^2 + hoh(:,7).^2));
    title('Satellite Orbit Radius');
    xlabel('Time (s)');
    ylabel('Radius (m)');
    figure(figs(3));
    subplot(2,1,1);
    plot(hoh(:,1), hoh(:,2));
    title('Engine burns');
    xlabel('Time (s)');
    ylabel('X delta-V (m/s)');
    legend('X');
    subplot(2,1,2)
    plot(hoh(:,1), hoh(:,3));
    xlabel('Time (s)')
    ylabel('Y delta-V (m/s)');
    legend('Y');
    
    % draw orbit diagram
    figure(figs(1));
    hold on;
    circle(0, 0, 6.357e6, 'FaceColor', 'g');
    circle(0, 0, sqrt(hoh(1,6)^2 + hoh(1,7)^2), 'EdgeColor', 'r');
    line([0, 0], [-6.357e6, 6.357e6], 'Color', 'c');
    line([-6.357e6, 6.357e6], [0, 0], 'Color', 'c');
    circle(0, 0, sqrt(hoh(length(hoh),6)^2 + hoh(length(hoh),7)^2), 'EdgeColor', 'b');
    line(hoh(:,6), hoh(:,7), 'Color', 'k');
    
    title('Satellite Orbit Diagram');
    grid on;
    axis equal;
    
    dcm = datacursormode(figs(1));
    datacursormode on;
    dcm.DisplayStyle = 'window';
    set(dcm.Figure, 'UserData', ...
        cellfun(@(h) sprintf('%g m/s @ %g degrees', sqrt(h(2)^2 + h(3)^2), 180/pi * atan2(h(3), h(2))), ...
        num2cell(hoh, 2), 'UniformOutput', false));
    set(dcm, 'UpdateFcn', @path_datacursor);
end

function circle(x, y, r, varargin)
    rectangle('Position', [x-r, y-r, r*2, r*2], 'Curvature', [1, 1], varargin{:});
end