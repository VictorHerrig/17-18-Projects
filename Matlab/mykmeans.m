function [C, V] =   mykmeans(n, k)
    centroids = n(randsample(length(n), k),:);
    n = [n,ones(length(n), 1)];
    pre = zeros(length(n), 1);
    while(any(pre ~= n(:,3)))
        pre = n(:,3);
        [~, index] = min(pdist2(centroids, n(:,1:2)));
        n(:, 3) = index;
        for i = 1:k
            centroids(i,:) = mean(n(n(:,3) == i,1:2));
        end
    end
    C = [(1:k)',centroids];
    V = n(:,3);
    colors = ['r', 'b', 'c', 'm', 'y', 'k', 'w'];
    for i = 1:k
        scatter(n(V == i, 1), n(V == i, 2), colors(mod(i, 7) + 1))
        hold on;
    end
    plot(C(:,2), C(:,3), '^g', 'markerfacecolor', 'g', 'MarkerSize', 10)
    hold off;
end